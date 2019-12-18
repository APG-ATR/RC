//! Ported from closure compiler.
pub use self::dce::dce;
use self::expr::SimplifyExpr;
use crate::{pass::Pass, util::*};
use ast::*;
use swc_common::{Fold, FoldWith, DUMMY_SP};

mod dce;
mod expr;
#[cfg(test)]
mod tests;

/// Not intended for general use. Use [simplifier] instead.
///
/// Ported from `PeepholeFoldConstants` of google closure compler.
pub fn expr_simplifier() -> impl Pass + 'static {
    SimplifyExpr
}

/// Ported from `PeepholeRemoveDeadCode` and `PeepholeFoldConstants` of google
/// closure compiler.
pub fn simplifier() -> impl Pass + 'static {
    chain!(expr_simplifier(), dce())
}

// impl Fold<Stmt> for Simplify {
//     fn fold(&mut self, stmt: Stmt) -> Stmt {
//         stmt.fold_children(&mut FoldConst)
//     }
// }
