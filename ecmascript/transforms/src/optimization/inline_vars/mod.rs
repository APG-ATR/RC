use crate::{
    pass::Pass,
    scope::ScopeKind,
    util::{id, undefined, DestructuringFinder, Id, StmtLike},
};
use ast::*;
use fxhash::FxHashMap;
use serde::Deserialize;
use std::{
    cell::{Cell, Ref, RefCell},
    collections::hash_map::Entry,
};
use swc_common::{fold::VisitWith, util::move_map::MoveMap, Fold, FoldWith, DUMMY_SP};

#[cfg(test)]
mod tests;

/// Ported from [`InlineVariables`](https://github.com/google/closure-compiler/blob/master/src/com/google/javascript/jscomp/InlineVariables.java)
/// of the google closure compiler.
pub fn inline_vars(_: Config) -> impl 'static + Pass {
    Inline {
        scope: Default::default(),
        top_level: true,
    }
}

#[derive(Debug, Default, Deserialize)]
pub struct Config {
    #[serde(default)]
    pub locals_only: bool,
}

#[derive(Debug, Default)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    kind: ScopeKind,
    /// Stored only if value is statically known.
    vars: RefCell<FxHashMap<Id, VarInfo>>,
}

#[derive(Debug)]
struct VarInfo {
    /// Count of usage.
    cnt: Cell<usize>,
    value: Expr,
}

#[derive(Debug, Default)]
struct Inline<'a> {
    scope: Scope<'a>,
    top_level: bool,
}

impl Inline<'_> {
    fn child<T, F>(&mut self, kind: ScopeKind, op: F) -> T
    where
        F: for<'any> FnOnce(&mut Inline<'any>) -> T,
    {
        let mut c = Inline {
            scope: Scope {
                parent: Some(&self.scope),
                kind,
                vars: Default::default(),
            },
            top_level: false,
        };

        op(&mut c)
    }
}

impl Scope<'_> {
    fn find(&self, i: &Ident) -> Option<Ref<Expr>> {
        if self.vars.borrow().get(&id(i)).is_none() {
            return self.parent.and_then(|p| p.find(i)).and_then(|e| match *e {
                Expr::This(..) => None,
                _ => Some(e),
            });
        }

        let r = Ref::map(self.vars.borrow(), |vars| {
            //
            let var_info = vars.get(&id(i)).unwrap();

            &var_info.value
        });
        Some(r)
    }

    fn take_var(&mut self, i: &Ident) -> Option<VarInfo> {
        match self.vars.get_mut().entry(id(i)) {
            Entry::Occupied(o) => Some(o.remove()),
            Entry::Vacant(..) => None,
        }
    }

    fn remove(&self, i: &Ident) {
        fn rem(s: &Scope, i: Id) {
            s.vars.borrow_mut().remove(&i);

            match s.parent {
                Some(ref p) => rem(p, i),
                _ => {}
            }
        }

        rem(self, id(i))
    }
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
        let e = e.fold_children(self);

        if e.op == op!("=") {
            match e.left {
                PatOrExpr::Pat(box Pat::Ident(ref i)) => self.store(i, &e.right),
                _ => {}
            }
        }

        e
    }
}

impl Fold<VarDecl> for Inline<'_> {
    fn fold(&mut self, v: VarDecl) -> VarDecl {
        let v = v.fold_children(self);

        for decl in &v.decls {
            match decl.name {
                Pat::Ident(ref i) => match decl.init {
                    Some(ref e) => self.store(i, e),
                    None => self.store(i, &*undefined(DUMMY_SP)),
                },
                _ => {}
            }
        }

        v
    }
}

impl Fold<Expr> for Inline<'_> {
    fn fold(&mut self, e: Expr) -> Expr {
        let e = e.fold_children(self);

        match e {
            Expr::Ident(ref i) => {
                if let Some(e) = self.scope.find(i) {
                    return e.clone();
                }
            }
            _ => {}
        }

        e
    }
}

impl Inline<'_> {
    fn should_store(&self, e: &Expr) -> bool {
        match e {
            Expr::Lit(..) | Expr::Ident(..) | Expr::This(..) => true,
            _ => false,
        }
    }

    fn store(&mut self, i: &Ident, e: &Expr) {
        if self.should_store(e) {
            self.scope.vars.borrow_mut().insert(
                id(i),
                VarInfo {
                    cnt: Default::default(),
                    value: e.clone(),
                },
            );
        } else {
            self.scope.remove(i)
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

        // Inline variables
        let stmts = stmts.fold_children(self);

        stmts.move_flat_map(|stmt| {
            // Remove unused variables

            Some(match stmt.try_into_stmt() {
                Ok(stmt) => T::from_stmt(match stmt {
                    Stmt::Decl(Decl::Var(mut var)) => {
                        // This variable is block scoped, and if current block does not use the
                        // variable, we can safely remove it.
                        let is_block_scoped =
                            var.kind == VarDeclKind::Let || var.kind == VarDeclKind::Const;

                        if is_block_scoped || (var.kind == VarDeclKind::Var && !top_level) {
                            var.decls = var.decls.move_flat_map(|decl| {
                                match decl.name {
                                    Pat::Ident(ref i) => {
                                        let var = if let Some(var) = self.scope.take_var(i) {
                                            var
                                        } else {
                                            return Some(decl);
                                        };

                                        if var.cnt.take() == 0 {
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
                        }

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
