use crate::util::ident::IdentLike;
use ast::*;
use swc_common::{Visit, VisitWith};

/// Finds all idents of variable
pub(crate) struct Preventer<'a, I: IdentLike> {
    pub found: &'a mut Vec<I>,
}

impl<'a, I: IdentLike> Visit<Expr> for Preventer<'a, I> {
    fn visit(&mut self, e: &Expr) {
        match e {
            Expr::Invalid(_) | Expr::Lit(_) | Expr::This(_) | Expr::Ident(_) => return,
            Expr::Member(_) => {}

            _ => e.visit_children(self),
        }
    }
}

impl<'a, I: IdentLike> Visit<Ident> for Preventer<'a, I> {
    fn visit(&mut self, i: &Ident) {
        self.found.push(I::from_ident(i));
    }
}
