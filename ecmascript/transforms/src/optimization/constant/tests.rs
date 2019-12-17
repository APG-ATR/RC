//! Copied from test suite of the google closure compiler.

use super::constant_propagator;

macro_rules! to {
    ($name:ident,$src:literal, $expected:literal) => {
        test!(
            Default::default(),
            |_| constant_propagator(),
            $name,
            $src,
            $expected
        );
    };
}

to!(
    simple_num_add,
    "var a = 1;
var b = 2;
var c = a + b",
    "var a = 1;
var b = 2;
var c = 1 + 2"
);
