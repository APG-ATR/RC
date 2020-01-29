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
    Lit(Lit),
    B(B),
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
        println!("Fold<B>");
        node.called = true;
        node
    }
}

#[test]
fn single() {
    let e = Expr {
        node: ExprKind::B(B { called: false }),
    };

    let e = e.fold_with(&mut LitFold);

    assert_eq!(
        e,
        Expr {
            node: ExprKind::B(B { called: true }),
        }
    )
}

#[test]
fn single_type() {
    let e = Expr {
        node: ExprKind::B(B { called: false }),
    };

    let mut folder = &mut (&mut LitFold as &mut dyn Fold<B>);

    let e = e.fold_with(&mut folder);

    assert_eq!(
        e,
        Expr {
            node: ExprKind::B(B { called: true }),
        }
    )
}

#[test]
fn double() {
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

#[test]
fn double_type() {
    let e = Expr {
        node: ExprKind::Lit(Lit::B(B { called: false })),
    };

    let mut folder = &mut (&mut LitFold as &mut Fold<B>);

    let e = e.fold_with(&mut folder);

    assert_eq!(
        e,
        Expr {
            node: ExprKind::Lit(Lit::B(B { called: true })),
        }
    )
}
