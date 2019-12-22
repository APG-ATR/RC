use ast::Ident;
use swc_atoms::JsWord;
use swc_common::{Span, SyntaxContext};

pub trait IdentLike: Sized {
    fn from_ident(i: &Ident) -> Self;
}

impl IdentLike for (JsWord, Span) {
    #[inline]
    fn from_ident(i: &Ident) -> Self {
        (i.sym.clone(), i.span)
    }
}

impl IdentLike for (JsWord, SyntaxContext) {
    #[inline]
    fn from_ident(i: &Ident) -> Self {
        (i.sym.clone(), i.span.ctxt())
    }
}

impl IdentLike for Ident {
    #[inline]
    fn from_ident(i: &Ident) -> Self {
        Ident::new(i.sym.clone(), i.span)
    }
}
