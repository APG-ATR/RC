//! Copied from test suite of the google closure compiler.

use super::constant_propagator;
use crate::optimization::expr_simplifier;
use swc_common::chain;

fn fold(src: &str, expected: &str) {
    test_transform!(
        Default::default(),
        |_| chain!(constant_propagator(), expr_simplifier()),
        src,
        expected
    );
}
