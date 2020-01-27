#![feature(specialization)]

use std::{cell::Cell, rc::Rc};
use swc_common::{Fold, FoldWith, VisitWith};

pub trait AssertFold<T>: Fold<T> {}

// check for trait bound

pub struct LitFold;

impl AssertFold<Expr> for LitFold {}

impl AssertFold<ExprKind> for LitFold {}

#[derive(Debug, Fold, PartialEq)]
pub struct Expr {
    pub node: ExprKind,
}

#[derive(Debug, Fold, PartialEq)]
#[fold(dynamic)]
pub enum ExprKind {
    RecursiveBound(Box<Expr>),
    Rec2(Vec<Option<Box<Expr>>>),

    Lit(Lit),
}

#[derive(Debug, Fold, PartialEq)]
#[fold(dynamic)]
pub enum Lit {
    A,
    B(B),
}

#[derive(Debug, Fold, PartialEq)]
pub struct B {
    called: bool,
}

impl Fold<B> for LitFold {
    fn fold(&mut self, mut node: B) -> B {
        node.called = true;
        node
    }
}

#[test]
fn test() {
    let e = Expr {
        node: ExprKind::Lit(Lit::B(B { called: false })),
    };

    let e = e.fold_with(&mut LitFold);

    assert_eq!(
        e,
        Expr {
            node: ExprKind::Lit(Lit::B(B { called: true })),
        }
    )
}
