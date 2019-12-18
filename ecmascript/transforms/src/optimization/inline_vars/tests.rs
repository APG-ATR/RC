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
