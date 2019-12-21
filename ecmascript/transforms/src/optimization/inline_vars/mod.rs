use crate::{
    pass::Pass,
    scope::ScopeKind,
    util::{id, undefined, Id, StmtLike},
};
use ast::*;
use fxhash::FxHashMap;
use serde::Deserialize;
use std::{
    cell::{RefCell, RefMut},
    collections::hash_map::Entry,
};
use swc_common::{util::move_map::MoveMap, Fold, FoldWith, DUMMY_SP};

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
        },
        top_level: true,
    }
}

#[derive(Debug, Default, Deserialize)]
pub struct Config {
    #[serde(default)]
    pub locals_only: bool,
}

#[derive(Debug)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    kind: ScopeKind,
    /// Stored only if value is statically known.
    vars: RefCell<FxHashMap<Id, VarInfo>>,
}

#[derive(Debug)]
struct VarInfo {
    /// Count of usage.
    cnt: usize,
    value: Option<Expr>,
}

#[derive(Debug)]
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
    /// Find a scope with kind == ScopeKind::Fn
    fn find_fn_scope(&self) -> Option<&Self> {
        match self.kind {
            ScopeKind::Fn => Some(self),
            _ => self.parent.and_then(|p| p.find_fn_scope()),
        }
    }

    fn scope_for(&self, i: &Ident) -> Option<&Self> {
        match self.vars.borrow().get(&id(i)) {
            Some(..) => Some(self),
            None => self.parent.and_then(|p| p.scope_for(i)),
        }
    }

    fn find(&self, i: &Ident) -> Option<RefMut<VarInfo>> {
        if self.vars.borrow().get(&id(i)).is_none() {
            let r = self.parent.and_then(|p| p.find(i))?;
            return match r.value {
                Some(Expr::This(..)) => None,
                _ => Some(r),
            };
        }

        let r = RefMut::map(self.vars.borrow_mut(), |vars| {
            //
            let var_info = vars.get_mut(&id(i)).unwrap();

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
        let e = e.fold_children(self);

        match e.left {
            PatOrExpr::Pat(box Pat::Ident(ref i)) => {
                if e.op == op!("=") {
                    self.store(i, &e.right, None)
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
        let v = v.fold_children(self);

        for decl in &v.decls {
            match decl.name {
                Pat::Ident(ref i) => match decl.init {
                    Some(ref e) => self.store(i, e, Some(v.kind)),
                    None => self.store(i, &*undefined(DUMMY_SP), Some(v.kind)),
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
                if let Some(mut var) = self.scope.find(i) {
                    if let Some(ref e) = (*var).value {
                        println!("inline: {}", i.sym);

                        return e.clone();
                    } else {
                        println!("cnt++; {}: usage", i.sym);
                        var.cnt += 1;
                    }
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

    fn store(&mut self, i: &Ident, e: &Expr, kind: Option<VarDeclKind>) {
        if self.should_store(e) {
            match kind {
                // Not hoisted
                Some(VarDeclKind::Let) | Some(VarDeclKind::Const) => {
                    self.scope.vars.borrow_mut().insert(
                        id(i),
                        VarInfo {
                            cnt: Default::default(),
                            value: Some(e.clone()),
                        },
                    );
                }

                // Hoisted
                Some(VarDeclKind::Var) => {
                    if let Some(fn_scope) = self.scope.find_fn_scope() {
                        fn_scope.vars.borrow_mut().insert(
                            id(i),
                            VarInfo {
                                cnt: Default::default(),
                                value: Some(e.clone()),
                            },
                        );
                    }
                }

                None => {
                    if let Some(scope) = self.scope.scope_for(i) {
                        if let Some(v) = scope.vars.borrow_mut().get_mut(&id(i)) {
                            v.cnt += 1;
                            println!("cnt++; {}; store: assign", i.sym)
                        }
                    }
                }
            }
        } else {
            if let Some(mut info) = self.scope.find(i) {
                println!("cnt++; {}; store: {:?}", i.sym, kind);
                info.cnt += 1;
                (*info).value = None;
            }
        }
    }

    fn remove(&mut self, i: &Ident) {
        if let Some(mut info) = self.scope.find(i) {
            println!("inline_vars: {}: remove", i.sym);
            info.cnt += 1;
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

        // Inline variables
        let stmts = stmts.fold_children(self);

        stmts.move_flat_map(|stmt| {
            // Remove unused variables

            Some(match stmt.try_into_stmt() {
                Ok(stmt) => T::from_stmt(match stmt {
                    Stmt::Block(BlockStmt { ref stmts, .. }) if stmts.is_empty() => return None,
                    Stmt::Empty(..) => return None,

                    Stmt::Decl(Decl::Var(mut var)) => {
                        var.decls = var.decls.move_flat_map(|decl| {
                            match decl.name {
                                Pat::Ident(ref i) => {
                                    let var = if let Some(var) = self.scope.take_var(i) {
                                        println!("inline_vars: {}: {}", i.sym, var.cnt);
                                        var
                                    } else {
                                        println!("inline_vars: {}: no var", i.sym);

                                        return Some(decl);
                                    };

                                    if var.cnt == 0 {
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
