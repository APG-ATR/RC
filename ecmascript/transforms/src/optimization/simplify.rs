//! Ported from closure compiler.
pub use self::dce::dce;
use self::expr::SimplifyExpr;
use crate::pass::Pass;
use swc_common::{chain, Fold};

pub mod dce;
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
    Simplifier
}

struct Simplifier;

impl Fold<Progrqm> for Simplifier {
    fn fold(&mut self, p: Progrqm) -> Progrqm {
        p.fold_with(&mut expr_simplifier()).fold_with(&mut dce())
    }
}
