use crate::{
    pass::Pass,
    scope::ScopeKind,
    util::{id, ident::IdentLike, undefined, DestructuringFinder, ExprExt, Id, StmtLike},
};
use ast::*;
use fxhash::FxHashMap;
use serde::Deserialize;
use std::{
    cell::{RefCell, RefMut},
    collections::hash_map::Entry,
    mem::replace,
};
use swc_atoms::JsWord;
use swc_common::{
    fold::VisitWith, util::move_map::MoveMap, Fold, FoldWith, Spanned, SyntaxContext, DUMMY_SP,
};

mod hoister;
#[cfg(test)]
mod tests;

macro_rules! check {
    ($i:expr) => {{
        if $i.scope.id != 0 {
            assert!(
                $i.scope.parent.is_some(),
                "Scope({}) does not have parent",
                $i.scope.id
            );
        }
    }};
}

/// Scope id generator.
#[derive(Debug, Clone, Default)]
struct Gen(usize);
impl Gen {
    #[inline]
    fn get(&mut self) -> usize {
        self.0 += 1;
        self.0
    }
}

/// Ported from [`InlineVariables`](https://github.com/google/closure-compiler/blob/master/src/com/google/javascript/jscomp/InlineVariables.java)
/// of the google closure compiler.
pub fn inline_vars(_: Config) -> impl 'static + Pass {
    Inline {
        scope: Scope {
            id: 0,
            parent: None,
            // This is important.
            kind: ScopeKind::Block,
            vars: Default::default(),
            children: Default::default(),
        },
        phase: Phase::Analysis,
        top_level: true,
        id_gen: Default::default(),
    }
}

#[derive(Debug, Default, Deserialize)]
pub struct Config {
    #[serde(default)]
    pub locals_only: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Phase {
    Analysis,
    Inlining,
    // CleanUp.
}

/// Reason that we should inline a variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Reason {
    SingleUse,
    Cheap,
}

#[derive(Debug)]
struct Scope<'a> {
    id: usize,
    parent: Option<&'a Scope<'a>>,
    kind: ScopeKind,
    /// Stored only if value is statically known.
    vars: RefCell<FxHashMap<Id, VarInfo>>,
    children: RefCell<Vec<Scope<'static>>>,
}

#[derive(Debug, Default)]
struct VarInfo {
    /// Count of usage.
    usage: i16,
    assign: i16,
    no_inline: bool,
    value: Option<Expr>,
}

#[derive(Debug)]
struct Inline<'a> {
    phase: Phase,
    scope: Scope<'a>,
    top_level: bool,
    id_gen: Gen,
}

impl Inline<'_> {
    fn child<T, F>(&mut self, kind: ScopeKind, op: F) -> T
    where
        F: for<'any> FnOnce(&mut Inline<'any>) -> T,
    {
        check!(self);

        match self.phase {
            Phase::Analysis => {
                let id = self.id_gen.get();

                let (res, vars, children) = {
                    let mut c = Inline {
                        scope: Scope {
                            id,
                            parent: Some(&self.scope),
                            kind,
                            vars: Default::default(),
                            children: Default::default(),
                        },
                        phase: self.phase,
                        top_level: false,
                        id_gen: self.id_gen.clone(),
                    };

                    let res = op(&mut c);

                    (res, c.scope.vars, c.scope.children)
                };

                self.scope.children.borrow_mut().push(Scope {
                    id,
                    parent: None,
                    kind,
                    vars,
                    children,
                });

                res
            }

            Phase::Inlining => {
                let mut scope = self.scope.children.get_mut().remove(0);
                scope.parent = Some(&self.scope);

                assert_eq!(kind, scope.kind);

                let mut c = Inline {
                    scope,
                    phase: self.phase,
                    top_level: false,
                    id_gen: self.id_gen.clone(),
                };

                op(&mut c)
            }
        }
    }
}

impl Scope<'_> {
    /// Find a scope with kind == ScopeKind::Fn
    fn find_fn_scope(&self) -> Option<&Self> {
        match self.kind {
            ScopeKind::Fn => Some(self),
            _ => self.parent.and_then(|p| p.find_fn_scope()),
        }
    }

    fn scope_for<I>(&self, i: &I) -> Option<&Self>
    where
        I: IdentLike,
    {
        match self.vars.borrow().get(&i.to_id()) {
            Some(..) => Some(self),
            None => self.parent.and_then(|p| p.scope_for(i)),
        }
    }

    fn find<I>(&self, i: &I) -> Option<RefMut<VarInfo>>
    where
        I: IdentLike,
    {
        //println!("          scope.find:");

        if self.vars.borrow().get(&i.to_id()).is_none() {
            //println!("              none");

            let r = self.parent.and_then(|p| p.find(i))?;
            return match r.value {
                Some(Expr::Invalid(..)) => unreachable!(),
                Some(Expr::This(..)) => None,
                _ => Some(r),
            };
        }

        //println!("              some");

        let r = RefMut::map(self.vars.borrow_mut(), |vars| {
            //
            let var_info = vars.get_mut(&i.to_id()).unwrap();

            var_info
        });
        match r.value {
            Some(Expr::Invalid(..)) => unreachable!(),
            _ => {}
        }
        Some(r)
    }

    fn take_var(&self, i: &Ident) -> Option<VarInfo> {
        let var = match self.vars.borrow_mut().entry(id(i)) {
            Entry::Occupied(o) => Some(o.remove()),
            Entry::Vacant(..) => self.parent.and_then(|p| p.take_var(i)),
        }?;

        match var.value {
            Some(Expr::Invalid(..)) => unreachable!(),
            _ => {}
        }

        Some(var)
    }

    //    fn remove(&self, i: &Ident) {
    //        fn rem(s: &Scope, i: Id) {
    //            s.vars.borrow_mut().remove(&i);
    //
    //            match s.parent {
    //                Some(ref p) => rem(p, i),
    //                _ => {}
    //            }
    //        }
    //
    //        rem(self, id(i))
    //    }
}

impl Fold<UpdateExpr> for Inline<'_> {
    fn fold(&mut self, e: UpdateExpr) -> UpdateExpr {
        match *e.arg {
            Expr::Ident(ref i) => self.prevent_inline(i),

            _ => {}
        }

        let e = e.fold_children(self);

        e
    }
}

impl Fold<Function> for Inline<'_> {
    fn fold(&mut self, f: Function) -> Function {
        check!(self);

        let res = self.child(ScopeKind::Fn, |folder| {
            // Hoist vars
            if folder.phase == Phase::Analysis {
                let vars = hoister::find_vars(&f);

                for i in vars {
                    folder.store(&i, &mut *undefined(DUMMY_SP), Some(VarDeclKind::Var));
                }
            }

            // Handle function
            Function {
                params: f.params.fold_with(folder),
                body: match f.body {
                    Some(bs) => Some(bs.fold_children(folder)),
                    None => None,
                },
                ..f
            }
        });

        res
    }
}

impl Fold<BlockStmt> for Inline<'_> {
    fn fold(&mut self, s: BlockStmt) -> BlockStmt {
        check!(self);

        self.child(ScopeKind::Block, |c| s.fold_children(c))
    }
}

impl Fold<SwitchCase> for Inline<'_> {
    fn fold(&mut self, s: SwitchCase) -> SwitchCase {
        check!(self);

        self.child(ScopeKind::Block, |c| s.fold_children(c))
    }
}

impl Fold<AssignExpr> for Inline<'_> {
    fn fold(&mut self, e: AssignExpr) -> AssignExpr {
        check!(self);

        let mut e = e.fold_children(self);

        match e.left {
            PatOrExpr::Pat(box Pat::Ident(ref i)) => {
                if e.op == op!("=") {
                    self.store(i, &mut e.right, None);
                } else {
                    self.prevent_inline(i)
                }
            }

            _ => {}
        }

        e
    }
}

impl Fold<VarDecl> for Inline<'_> {
    fn fold(&mut self, v: VarDecl) -> VarDecl {
        check!(self);

        let mut v = v.fold_children(self);

        for decl in &mut v.decls {
            match decl.name {
                Pat::Ident(ref i) => match decl.init {
                    Some(ref mut e) => {
                        self.store(i, e, Some(v.kind));
                    }
                    None => self.store(i, &mut *undefined(DUMMY_SP), Some(v.kind)),
                },
                _ => {}
            }
        }

        v
    }
}

impl Fold<ForOfStmt> for Inline<'_> {
    fn fold(&mut self, s: ForOfStmt) -> ForOfStmt {
        check!(self);

        let s = s.fold_children(self);

        if self.phase == Phase::Analysis {
            let mut found: Vec<(JsWord, SyntaxContext)> = vec![];
            let mut v = DestructuringFinder { found: &mut found };
            s.left.visit_with(&mut v);

            for id in found {
                let var = self.scope.find(&id);
                if let Some(mut var) = var {
                    var.no_inline = true;
                }
            }
        }

        s
    }
}

impl Fold<ForInStmt> for Inline<'_> {
    fn fold(&mut self, s: ForInStmt) -> ForInStmt {
        check!(self);

        let s = s.fold_children(self);

        if self.phase == Phase::Analysis {
            let mut found: Vec<(JsWord, SyntaxContext)> = vec![];
            let mut v = DestructuringFinder { found: &mut found };
            s.left.visit_with(&mut v);

            for id in found {
                let var = self.scope.find(&id);
                if let Some(mut var) = var {
                    var.no_inline = true;
                }
            }
        }

        s
    }
}

impl Fold<Expr> for Inline<'_> {
    fn fold(&mut self, e: Expr) -> Expr {
        check!(self);

        let e = e.fold_children(self);

        match e {
            Expr::Ident(ref i) => {
                match self.phase {
                    Phase::Analysis => {
                        if let Some(mut var) = self.scope.find(i) {
                            println!("cnt++; {}: usage", i.sym);
                            var.usage += 1;
                        }
                    }

                    Phase::Inlining => {
                        let e = if let Some(mut var) = self.scope.find(i) {
                            if var.no_inline {
                                return e;
                            }

                            println!(
                                "Scope({}): inlining: {}: found var: {:?}",
                                self.scope.id, i.sym, var
                            );
                            if var.value.is_some() {
                                // Variable is inlined
                                var.usage -= 1;
                            }

                            if let Some(ref e) = var.value {
                                Some(e.clone())
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                        if let Some(e) = e {
                            // Inline again if required.
                            return e.fold_with(self);
                        }
                    }
                }
            }
            _ => {}
        }

        e
    }
}

impl Inline<'_> {
    fn should_store(&self, i: &Id, e: &Expr) -> Option<Reason> {
        println!("  should store:");

        if self.phase == Phase::Analysis {
            return Some(Reason::Cheap);
        }

        if self.phase == Phase::Inlining {
            if let Some(ref v) = self.scope.find(i) {
                println!(
                    "      Scope({}): found var: {}: {:?}",
                    self.scope.id, i.0, v
                );
                if v.no_inline {
                    return None;
                }

                if v.usage <= 1 {
                    return Some(Reason::SingleUse);
                }
            }

            println!("          not handled");
        }

        match e {
            Expr::Lit(..) | Expr::Ident(..) | Expr::This(..) => return Some(Reason::Cheap),
            _ => {}
        }

        None
    }

    fn store<I>(&mut self, i: &I, e: &mut Expr, kind: Option<VarDeclKind>)
    where
        I: IdentLike,
    {
        let i = i.to_id();
        println!(
            "Scope({}): {:?}: {}: store {:?}",
            self.scope.id, self.phase, i.0, kind
        );
        let span = e.span();

        let reason = if let Some(reason) = self.should_store(&i, e) {
            println!("  reason: {:?}", reason);
            reason
        } else {
            println!("  no_inline");
            if let Some(mut info) = self.scope.find(&i) {
                info.no_inline = true;
                (*info).value = None;
            }

            return;
        };

        let value = if self.phase == Phase::Inlining {
            Some(if reason == Reason::SingleUse {
                println!("  Taking");
                replace(e, Expr::Invalid(Invalid { span }))
            } else {
                e.clone()
            })
        } else {
            None
        };

        match value {
            Some(Expr::Invalid(..)) => unreachable!(),
            _ => {}
        }

        match kind {
            // Not hoisted
            Some(VarDeclKind::Let) | Some(VarDeclKind::Const) => {
                self.scope.vars.borrow_mut().entry(i).or_default().value = value;
            }

            // Hoisted
            Some(VarDeclKind::Var) => {
                if let Some(fn_scope) = self.scope.find_fn_scope() {
                    fn_scope.vars.borrow_mut().entry(i).or_default().value = value;
                }
            }

            None => {
                if let Some(scope) = self.scope.scope_for(&i) {
                    if scope.id != self.scope.id {
                        self.prevent_inline(&i)
                    } else {
                        if let Some(v) = scope.vars.borrow_mut().get_mut(&i) {
                            v.assign += 1;
                            println!("cnt++; {}; store: assign", i.0)
                        }
                    }
                }
            }
        }
    }

    fn prevent_inline<I>(&mut self, i: &I)
    where
        I: IdentLike,
    {
        if let Some(mut info) = self.scope.find(i) {
            info.no_inline = true;
            (*info).value = None;
        }
    }
}

impl<T: StmtLike> Fold<Vec<T>> for Inline<'_>
where
    T: FoldWith<Self>,
    Vec<T>: FoldWith<Self>,
{
    fn fold(&mut self, stmts: Vec<T>) -> Vec<T> {
        let top_level = self.top_level;
        self.top_level = false;

        let stmts = stmts.fold_children(self);

        match self.phase {
            Phase::Analysis => {
                // println!("Scope({}): {:?}", self.scope.id, self.scope.vars);
                if top_level {
                    // println!("----- ----- ----- ----- -----");
                    self.phase = Phase::Inlining;
                    // Inline variables
                    stmts.fold_with(self)
                } else {
                    stmts
                }
            }
            Phase::Inlining => {
                stmts.move_flat_map(|stmt| {
                    // Remove unused variables

                    Some(match stmt.try_into_stmt() {
                        Ok(stmt) => T::from_stmt(match stmt {
                            Stmt::Block(BlockStmt { ref stmts, .. }) if stmts.is_empty() => {
                                return None
                            }
                            Stmt::Empty(..) => return None,

                            Stmt::Decl(Decl::Var(mut v)) => {
                                let kind = v.kind;

                                v.decls = v.decls.move_flat_map(|decl| {
                                    match decl.init {
                                        // Remove inlined variables (single usage).
                                        Some(box Expr::Invalid(..)) => {
                                            return None;
                                        }
                                        _ => {}
                                    }

                                    // If variable is used, we can't remove it.
                                    let var = match decl.name {
                                        Pat::Ident(ref i) => {
                                            if let Some(var) = self.scope.take_var(i) {
                                                if var.no_inline
                                                    || var.usage != 0
                                                    || var.assign != 0
                                                {
                                                    return Some(decl);
                                                }

                                                var
                                            } else {
                                                return Some(decl);
                                            }
                                        }
                                        // Be conservative
                                        _ => return Some(decl),
                                    };
                                    assert_eq!(var.usage, 0);
                                    assert!(!var.no_inline);

                                    // At here, variable is not used.
                                    if decl.init.is_none()
                                        || !decl.init.as_ref().unwrap().may_have_side_effects()
                                    {
                                        if match kind {
                                            VarDeclKind::Const | VarDeclKind::Let => true,
                                            VarDeclKind::Var if !top_level => true,
                                            _ => false,
                                        } {
                                            return None;
                                        }
                                    }

                                    Some(decl)
                                });

                                if v.decls.is_empty() {
                                    return None;
                                }

                                Stmt::Decl(Decl::Var(v))
                            }

                            _ => stmt,
                        }),
                        Err(item) => item,
                    })
                })
            }
        }
    }
}
