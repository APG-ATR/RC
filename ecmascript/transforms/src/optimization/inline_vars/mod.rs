use self::var::VarInfo;
use crate::{
    pass::Pass,
    scope::ScopeKind,
    util::{id, ident::IdentLike, undefined, DestructuringFinder, Id, StmtLike},
};
use ast::*;
use fxhash::FxHashMap;
use serde::Deserialize;
use std::{
    cell::{RefCell, RefMut},
    collections::hash_map::Entry,
};
use swc_atoms::JsWord;
use swc_common::{
    fold::VisitWith, util::move_map::MoveMap, Fold, FoldWith, SyntaxContext, DUMMY_SP,
};

mod hoister;
#[cfg(test)]
mod tests;
mod var;

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
        changed: false,
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
    /// Saving value of variables into the scope.
    Storage,
    Inlining,
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

#[derive(Debug)]
struct Inline<'a> {
    phase: Phase,
    changed: bool,
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
                        changed: false,
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

            Phase::Storage | Phase::Inlining => {
                let mut scope = self.scope.children.get_mut().remove(0);
                scope.parent = Some(&self.scope);

                assert_eq!(kind, scope.kind);

                let mut c = Inline {
                    scope,
                    phase: self.phase,
                    top_level: false,
                    changed: self.changed,
                    id_gen: self.id_gen.clone(),
                };

                let res = op(&mut c);

                self.changed |= c.changed;

                // Treat children as a ring. Note that we don't remove an empty block statement
                // to preserve scope order.
                self.scope.children.borrow_mut().push(Scope {
                    id: c.scope.id,
                    parent: None,
                    kind,
                    vars: c.scope.vars,
                    children: c.scope.children,
                });

                res
            }
        }
    }
}

impl Scope<'_> {
    fn drop_usage(&self, e: &Expr) {
        match e {
            Expr::Ident(i) => {
                if let Some(mut v) = self.find(i) {
                    v.usage -= 1;
                }
            }

            Expr::Assign(e) => {
                self.drop_assign(&e.left);
                self.drop_usage(&e.right);
            }

            _ => {}
        }
    }

    fn drop_assign(&self, p: &PatOrExpr) {
        match p {
            PatOrExpr::Pat(box p) => match p {
                Pat::Ident(i) => {
                    if let Some(mut v) = self.find(i) {
                        v.assign -= 1;
                    }
                }
                _ => {}
            },

            _ => {}
        }
    }

    fn is_root(&self) -> bool {
        self.parent.is_none()
    }

    fn root(&self) -> &Self {
        self.parent.map(|p| p.root()).unwrap_or(self)
    }

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
            return match r.value() {
                Some(&Expr::This(..)) => None,
                _ => Some(r),
            };
        }

        //println!("              some");

        let r = RefMut::map(self.vars.borrow_mut(), |vars| {
            //
            let var_info = vars.get_mut(&i.to_id()).unwrap();

            var_info
        });

        Some(r)
    }

    fn take_var(&self, i: &Ident) -> Option<VarInfo> {
        let var = match self.vars.borrow_mut().entry(id(i)) {
            Entry::Occupied(o) => Some(o.remove()),
            Entry::Vacant(..) => self.parent.and_then(|p| p.take_var(i)),
        }?;

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
            match folder.phase {
                Phase::Analysis => {
                    let vars = hoister::find_vars(&f);

                    for i in vars {
                        folder.store(&i, &mut *undefined(DUMMY_SP), Some(VarDeclKind::Var));
                    }
                }
                _ => {}
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
            PatOrExpr::Pat(box Pat::Ident(ref i)) => match self.phase {
                Phase::Analysis | Phase::Storage => {
                    if e.op == op!("=") {
                        self.store(i, &mut e.right, None);
                    } else {
                        self.prevent_inline(i)
                    }
                }
                Phase::Inlining => {}
            },

            _ => {}
        }

        e
    }
}

impl Fold<VarDecl> for Inline<'_> {
    fn fold(&mut self, v: VarDecl) -> VarDecl {
        check!(self);

        let id = self.scope.id;
        let kind = v.kind;
        let mut v = v.fold_children(self);

        v.decls = v.decls.move_flat_map(|mut decl| {
            match decl.init {
                Some(box Expr::Invalid(..)) => return None,
                _ => {}
            }

            if self.phase == Phase::Inlining {
                // If variable is used, we can't remove it.
                let var = match decl.name {
                    Pat::Ident(ref i) => {
                        let scope = match self.scope.scope_for(i) {
                            // We can't remove variables in top level
                            Some(v) if v.is_root() => return Some(decl),
                            Some(v) => v,
                            None => return Some(decl),
                        };

                        if scope
                            .find(i)
                            .as_ref()
                            .map(|var| var.assign == 0 && var.usage == 0)
                            .unwrap_or(false)
                        {
                            if let Some(ref e) = decl.init {
                                self.changed = true;
                                scope.drop_usage(&e);
                            }
                        }

                        if let Some(var) = scope.find(i) {
                            // println!("Scope({}, {}): {}: {:?}", scope_id, scope.id, i.sym, var);

                            var
                        } else {
                            return Some(decl);
                        }
                    }
                    // Be conservative
                    _ => return Some(decl),
                };

                if var.assign == 0 && var.usage == 0 {
                    println!("Scope({}): removing {:?} as it's not used", id, decl.name);
                    return None;
                }

                if var.no_inline() {
                    return Some(decl);
                }
            }

            //
            if self.phase != Phase::Inlining {
                match decl.name {
                    Pat::Ident(ref i) => match decl.init {
                        Some(ref mut e) => {
                            self.store(i, e, Some(kind));
                        }
                        None => self.store(i, &mut *undefined(DUMMY_SP), Some(kind)),
                    },
                    _ => {}
                }
            }

            Some(decl)
        });

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
                    var.prevent_inline()
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
                    var.prevent_inline()
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
            Expr::Ident(i) => {
                match self.phase {
                    Phase::Analysis => {
                        if let Some(mut var) = self.scope.find(&i) {
                            println!("cnt++; {}: usage", i.sym);
                            var.usage += 1;
                        }
                    }

                    Phase::Storage => {}

                    Phase::Inlining => {
                        let e: Option<Expr> = if let Some(var) = self.scope.find(&i) {
                            if var.no_inline() {
                                return Expr::Ident(i);
                            }

                            if let Some(e) = var.value() {
                                Some(e.clone())
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                        if let Some(e) = e {
                            println!("Scope({}): inlined '{}'", self.scope.id, i.sym);

                            self.changed = true;
                            self.scope.drop_usage(&Expr::Ident(i));

                            // Inline again if required.
                            return e.fold_with(self);
                        }
                    }
                }

                return Expr::Ident(i);
            }
            _ => {}
        }

        e
    }
}

impl Inline<'_> {
    fn should_store(&self, i: &Id, e: &Expr) -> Option<Reason> {
        //println!("  should store:");

        if self.phase == Phase::Analysis {
            return Some(Reason::Cheap);
        }

        if self.phase == Phase::Inlining {
            if let Some(ref v) = self.scope.find(i) {
                //println!(
                //    "      Scope({}): found var: {}: {:?}",
                //    self.scope.id, i.0, v
                //);
                if v.no_inline() {
                    return None;
                }

                if v.assign == 0 && v.usage == 1 {
                    return Some(Reason::SingleUse);
                }
            }

            //println!("          not handled");
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
        assert_ne!(
            self.phase,
            Phase::Inlining,
            "store() should not be called while inlining"
        );

        let scope_id = self.scope.id;
        let i = i.to_id();
        //println!(
        //    "Scope({}): {:?}: {}: store {:?}",
        //    self.scope.id, self.phase, i.0, kind
        //);

        let reason = if let Some(reason) = self.should_store(&i, e) {
            //println!("  reason: {:?}", reason);
            reason
        } else {
            //println!("  no_inline");
            if let Some(mut info) = self.scope.find(&i) {
                info.prevent_inline()
            }

            return;
        };

        let value = if self.phase == Phase::Inlining {
            Some(if reason == Reason::SingleUse {
                //                replace(e, Expr::Invalid(Invalid { span }))
                e.clone()
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
                self.scope
                    .vars
                    .borrow_mut()
                    .entry(i)
                    .or_insert_with(|| VarInfo::new(scope_id))
                    .set_value(value);
            }

            // Hoisted
            Some(VarDeclKind::Var) => {
                if let Some(fn_scope) = self.scope.find_fn_scope() {
                    let mut v = fn_scope.vars.borrow_mut();

                    let v = v.entry(i).or_insert_with(|| VarInfo::new(scope_id));

                    v.set_value(value);
                    if scope_id != v.scope_id() {
                        v.prevent_inline()
                    }
                } else {
                    let mut v = self.scope.root().vars.borrow_mut();
                    let v = v.entry(i).or_insert_with(|| VarInfo::new(scope_id));
                    if self.scope.is_root() {
                        v.set_value(value);
                    }
                    if scope_id != v.scope_id() {
                        v.prevent_inline()
                    }
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
        assert_ne!(
            self.phase,
            Phase::Inlining,
            "prevent_inline() should not be called while inlining"
        );

        if let Some(mut v) = self.scope.find(i) {
            v.prevent_inline()
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

        println!(
            "----- ----- ({}) {:?} ----- -----",
            self.scope.id, self.phase
        );

        let stmts = stmts.fold_children(self);

        match self.phase {
            Phase::Analysis => {
                // println!("Scope({}): {:?}", self.scope.id, self.scope.vars);
                if top_level {
                    self.phase = Phase::Storage;
                    // Inline variables
                    let stmts = stmts.fold_with(self);

                    self.phase = Phase::Inlining;
                    // Inline variables
                    let mut stmts = stmts;
                    loop {
                        stmts = stmts.fold_with(self);

                        if !self.changed {
                            break;
                        }
                        self.changed = false;
                    }
                    stmts
                } else {
                    stmts
                }
            }
            Phase::Storage => stmts,
            Phase::Inlining => {
                let is_root = self.scope.is_root();

                println!(
                    "----- ----- ({}) Removing vars ----- -----\n{:?}",
                    self.scope.id, self.scope.vars
                );

                stmts.move_flat_map(|stmt| {
                    // Remove unused variables

                    Some(match stmt.try_into_stmt() {
                        Ok(stmt) => T::from_stmt(match stmt {
                            //Stmt::Block(BlockStmt { ref stmts, .. }) if stmts.is_empty() => {
                            //    return None
                            //}
                            Stmt::Empty(..) => return None,

                            // We can't remove variables in top level
                            Stmt::Decl(Decl::Var(v)) if !is_root => {
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
