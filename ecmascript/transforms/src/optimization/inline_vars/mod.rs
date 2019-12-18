use crate::{
    pass::Pass,
    scope::ScopeKind,
    util::{id, Id},
};
use ast::*;
use hashbrown::HashMap;
use serde::Deserialize;
use std::cell::RefCell;
use swc_atoms::JsWord;
use swc_common::{Fold, FoldWith, SyntaxContext};

#[cfg(test)]
mod tests;

/// Ported from [`InlineVariables`](https://github.com/google/closure-compiler/blob/master/src/com/google/javascript/jscomp/InlineVariables.java)
/// of the google closure compiler.
pub fn inline_vars(_: Config) -> impl 'static + Pass {
    Inline {
        scope: Default::default(),
    }
}

#[derive(Debug, Default, Deserialize)]
pub struct Config {
    #[serde(default)]
    pub locals_only: bool,
}

#[derive(Debug, Default)]
struct Inline<'a> {
    scope: Scope<'a>,
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
                idents: Default::default(),
            },
        };

        op(&mut c)
    }
}

#[derive(Debug, Default)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    kind: ScopeKind,
    /// Stored only if value is statically known.
    idents: RefCell<HashMap<Id, Expr>>,
}

impl Scope<'_> {
    fn find(&self, i: &Ident) -> Option<Expr> {
        if let Some(e) = self
            .idents
            .borrow()
            .iter()
            .find(|e| (e.0).0 == i.sym && (e.0).1 == i.span.ctxt())
        {
            return Some(e.1.clone());
        }

        match self.parent {
            Some(ref p) => p.find(i),
            None => None,
        }
    }

    fn remove(&self, i: &Ident) {
        fn rem(s: &Scope, i: Id) {
            s.idents.borrow_mut().remove(&i);

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

impl Fold<AssignExpr> for Inline<'_> {
    fn fold(&mut self, e: AssignExpr) -> AssignExpr {
        let e = e.fold_children(self);

        if e.op == op!("=") {
            match e.left {
                PatOrExpr::Pat(box Pat::Ident(ref i)) => match *e.right {
                    Expr::Lit(..) | Expr::Ident(..) => {
                        self.scope.idents.get_mut().insert(id(i), *e.right.clone());
                    }
                    _ => self.scope.remove(i),
                },
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
                    Some(ref e @ box Expr::Lit(..)) | Some(ref e @ box Expr::Ident(..)) => {
                        self.scope.idents.get_mut().insert(id(i), *e.clone());
                    }
                    _ => self.scope.remove(i),
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
                    return e;
                }
            }
            _ => {}
        }

        e
    }
}
