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
    mem::replace,
};
use swc_atoms::JsWord;
use swc_common::{
    fold::VisitWith, util::move_map::MoveMap, Fold, FoldWith, Spanned, SyntaxContext, DUMMY_SP,
};

#[cfg(test)]
mod tests;

/// Ported from [`InlineVariables`](https://github.com/google/closure-compiler/blob/master/src/com/google/javascript/jscomp/InlineVariables.java)
/// of the google closure compiler.
pub fn inline_vars(_: Config) -> impl 'static + Pass {
    Inline {
        scope: Scope {
            parent: None,
            // This is important.
            kind: ScopeKind::Block,
            vars: Default::default(),
            children: Default::default(),
        },
        phase: Phase::Analysis,
        top_level: true,
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
    parent: Option<&'a Scope<'a>>,
    kind: ScopeKind,
    /// Stored only if value is statically known.
    vars: RefCell<FxHashMap<Id, VarInfo>>,
    children: RefCell<Vec<Scope<'static>>>,
}

#[derive(Debug)]
struct VarInfo {
    /// Count of usage.
    usage_cnt: usize,
    no_inline: bool,
    value: Option<Expr>,
}

#[derive(Debug)]
struct Inline<'a> {
    phase: Phase,
    scope: Scope<'a>,
    top_level: bool,
}

impl Inline<'_> {
    fn child<T, F>(&mut self, kind: ScopeKind, op: F) -> T
    where
        F: for<'any> FnOnce(&mut Inline<'any>) -> T,
    {
        match self.phase {
            Phase::Analysis => {
                let (res, vars, children) = {
                    let mut c = Inline {
                        scope: Scope {
                            parent: Some(&self.scope),
                            kind,
                            vars: Default::default(),
                            children: Default::default(),
                        },
                        phase: self.phase,
                        top_level: false,
                    };

                    let res = op(&mut c);

                    (res, c.scope.vars, c.scope.children)
                };

                self.scope.children.borrow_mut().push(Scope {
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

                let mut c = Inline {
                    scope,
                    phase: self.phase,
                    top_level: false,
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
        println!("          scope.find:");

        if self.vars.borrow().get(&i.to_id()).is_none() {
            println!("              none");

            let r = self.parent.and_then(|p| p.find(i))?;
            return match r.value {
                Some(Expr::This(..)) => None,
                _ => Some(r),
            };
        }

        println!("              some");

        let r = RefMut::map(self.vars.borrow_mut(), |vars| {
            //
            let var_info = vars.get_mut(&i.to_id()).unwrap();

            var_info
        });
        Some(r)
    }

    fn take_var(&self, i: &Ident) -> Option<VarInfo> {
        match self.vars.borrow_mut().entry(id(i)) {
            Entry::Occupied(o) => Some(o.remove()),
            Entry::Vacant(..) => self.parent.and_then(|p| p.take_var(i)),
        }
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

impl Fold<Function> for Inline<'_> {
    fn fold(&mut self, f: Function) -> Function {
        self.child(ScopeKind::Fn, |folder| Function {
            params: f.params.fold_with(folder),
            body: match f.body {
                Some(bs) => Some(bs.fold_children(folder)),
                None => None,
            },
            ..f
        })
    }
}

impl Fold<BlockStmt> for Inline<'_> {
    fn fold(&mut self, s: BlockStmt) -> BlockStmt {
        self.child(ScopeKind::Block, |c| s.fold_children(c))
    }
}

impl Fold<SwitchCase> for Inline<'_> {
    fn fold(&mut self, s: SwitchCase) -> SwitchCase {
        self.child(ScopeKind::Block, |c| s.fold_children(c))
    }
}

impl Fold<AssignExpr> for Inline<'_> {
    fn fold(&mut self, e: AssignExpr) -> AssignExpr {
        let mut e = e.fold_children(self);

        match e.left {
            PatOrExpr::Pat(box Pat::Ident(ref i)) => {
                if e.op == op!("=") {
                    self.store(i, &mut e.right, None)
                } else {
                    self.remove(i)
                }
            }

            _ => {}
        }

        e
    }
}

impl Fold<VarDecl> for Inline<'_> {
    fn fold(&mut self, v: VarDecl) -> VarDecl {
        let mut v = v.fold_children(self);

        for decl in &mut v.decls {
            match decl.name {
                Pat::Ident(ref i) => match decl.init {
                    Some(ref mut e) => self.store(i, e, Some(v.kind)),
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
        let e = e.fold_children(self);

        match e {
            Expr::Ident(ref i) => {
                if let Some(mut var) = self.scope.find(i) {
                    if let Some(ref e) = (*var).value {
                        println!("inline: {}", i.sym);

                        return e.clone();
                    } else {
                        println!("cnt++; {}: usage", i.sym);
                        var.usage_cnt += 1;
                    }
                }
            }
            _ => {}
        }

        e
    }
}

impl Inline<'_> {
    fn should_store(&self, i: &Ident, e: &Expr) -> Option<Reason> {
        println!(" should store:");

        if self.phase == Phase::Analysis {
            return Some(Reason::Cheap);
        }

        if self.phase == Phase::Inlining {
            if let Some(ref v) = self.scope.find(i) {
                println!("      found var");
                if v.no_inline {
                    return None;
                }

                if v.usage_cnt == 1 {
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

    fn store(&mut self, i: &Ident, e: &mut Expr, kind: Option<VarDeclKind>) {
        println!("{:?}: {}: store", self.phase, i.sym);
        let span = e.span();

        let reason = if let Some(reason) = self.should_store(i, e) {
            println!("  reason: {:?}", reason);
            reason
        } else {
            println!("  no_inline");
            if let Some(mut info) = self.scope.find(i) {
                info.no_inline = true;
                (*info).value = None;
            }

            return;
        };

        let value = if self.phase == Phase::Inlining {
            Some(if reason == Reason::SingleUse {
                println!("Taking an expression: {}", i.sym);
                replace(e, Expr::Invalid(Invalid { span }))
            } else {
                e.clone()
            })
        } else {
            None
        };

        match kind {
            // Not hoisted
            Some(VarDeclKind::Let) | Some(VarDeclKind::Const) => {
                self.scope.vars.borrow_mut().insert(
                    id(i),
                    VarInfo {
                        usage_cnt: Default::default(),
                        no_inline: false,
                        value,
                    },
                );
            }

            // Hoisted
            Some(VarDeclKind::Var) => {
                if let Some(fn_scope) = self.scope.find_fn_scope() {
                    fn_scope.vars.borrow_mut().insert(
                        id(i),
                        VarInfo {
                            usage_cnt: Default::default(),
                            no_inline: false,
                            value,
                        },
                    );
                }
            }

            None => {
                if let Some(scope) = self.scope.scope_for(i) {
                    if let Some(v) = scope.vars.borrow_mut().get_mut(&id(i)) {
                        v.usage_cnt += 1;
                        println!("cnt++; {}; store: assign", i.sym)
                    }
                }
            }
        }
    }

    fn remove(&mut self, i: &Ident) {
        if let Some(mut info) = self.scope.find(i) {
            println!("inline_vars: {}: remove", i.sym);
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
                println!("Vars: {:?}", self.scope.vars);
                if top_level {
                    println!("----- ----- ----- ----- -----");
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

                            Stmt::Decl(Decl::Var(mut var)) => {
                                var.decls = var.decls.move_flat_map(|decl| {
                                    match decl.name {
                                        Pat::Ident(ref i) => {
                                            let var = if let Some(
                                                var
                                                @
                                                VarInfo {
                                                    no_inline: false, ..
                                                },
                                            ) = self.scope.take_var(i)
                                            {
                                                println!(
                                                    "inline_vars: {}: {}",
                                                    i.sym, var.usage_cnt
                                                );
                                                var
                                            } else {
                                                return Some(decl);
                                            };

                                            if var.usage_cnt == 0 {
                                                None
                                            } else {
                                                Some(decl)
                                            }
                                        }
                                        _ => {
                                            // Be conservative
                                            Some(decl)
                                        }
                                    }
                                });

                                if var.decls.is_empty() {
                                    return None;
                                }

                                Stmt::Decl(Decl::Var(var))
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
