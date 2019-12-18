//! Copied from https://github.com/google/closure-compiler/blob/6ca3b62990064488074a1a8931b9e8dc39b148b3/test/com/google/javascript/jscomp/InlineVariablesTest.java

use super::inline_vars;

fn fold(src: &str, expected: &str) {
    test_transform!(
        ::swc_ecma_parser::Syntax::default(),
        |_| inline_vars(),
        src,
        expected,
        true
    )
}

/// Should not modify expression.
fn fold_same(s: &str) {
    fold(s, s)
}
