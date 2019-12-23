use ast::*;

/// Lives at here to prevent mistakes.
#[derive(Debug, Default)]
pub(super) struct VarInfo {
    /// Count of usage.
    pub usage: i16,
    pub assign: i16,
    no_inline: bool,
    value: Option<Expr>,
}

impl VarInfo {
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

    pub fn no_inline(&self) -> bool {
        self.no_inline
    }

    pub fn value(&self) -> Option<&Expr> {
        self.value.as_ref()
    }
}
