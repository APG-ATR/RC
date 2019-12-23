use crate::util::ExprExt;
use ast::*;

/// Lives at here to prevent mistakes.
#[derive(Debug)]
pub(super) struct VarInfo {
    /// Declared / assigned scope, not stored scope.
    scope_id: usize,
    /// Count of usage.
    pub usage: i16,
    pub assign: i16,
    no_inline: bool,
    value: Option<Expr>,
}

impl VarInfo {
    pub fn can_be_removed(&self) -> bool {
        self.assign == 0
            && self.usage == 0
            && self.value.is_some()
            && !self.value.as_ref().unwrap().may_have_side_effects()
    }

    pub const fn new(scope_id: usize) -> Self {
        VarInfo {
            scope_id,
            usage: 0,
            assign: 0,
            no_inline: false,
            value: None,
        }
    }

    pub const fn scope_id(&self) -> usize {
        self.scope_id
    }

    pub fn take_value(&mut self) -> Option<Expr> {
        self.value.take()
    }

    pub fn set_value(&mut self, e: Option<Expr>) {
        match e {
            Some(Expr::Invalid(..)) => unreachable!(),
            _ => {}
        }

        self.value = e
    }

    pub fn prevent_inline(&mut self) {
        self.no_inline = true;
        self.value = None;
    }

    pub const fn no_inline(&self) -> bool {
        self.no_inline
    }

    pub fn value(&self) -> Option<&Expr> {
        self.value.as_ref()
    }
}
