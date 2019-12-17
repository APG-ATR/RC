use crate::{
    pass::Pass,
    util::{is_literal, ExprExt, Value::Known},
};
use ast::*;
use hashbrown::HashMap;
use std::{cell::RefCell, f64::NAN};
use swc_atoms::{js_word, JsWord};
use swc_common::{Fold, FoldWith, Spanned, SyntaxContext};

#[cfg(test)]
mod tests;

pub fn constant_propagator() -> impl 'static + Pass {
    Const::default()
}

type Id = (JsWord, SyntaxContext);

fn id(i: &Ident) -> Id {
    (i.sym.clone(), i.span.ctxt())
}

#[derive(Debug, Default)]
struct Const<'a> {
    scope: Scope<'a>,
}

impl Const<'_> {
    fn child<T, F>(&mut self, op: F) -> T
    where
        F: for<'any> FnOnce(&mut Const<'any>) -> T,
    {
        let mut c = Const {
            scope: Scope {
                parent: Some(&self.scope),
                idents: Default::default(),
            },
        };

        op(&mut c)
    }
}

#[derive(Debug, Default)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    /// Stored only if value is statically known.
    idents: RefCell<HashMap<Id, Expr>>,
}

impl Scope<'_> {
    fn find(&self, i: &Ident) -> Option<Expr> {
        if let Some(e) = self
            .idents
            .borrow()
            .iter()
            .find(|e| (e.0).0 == i.sym && (e.0).1 == i.span.ctxt())
        {
            return Some(e.1.clone());
        }

        match self.parent {
            Some(ref p) => p.find(i),
            None => None,
        }
    }

    fn remove(&self, i: &Ident) {
        fn rem(s: &Scope, i: Id) {
            s.idents.borrow_mut().remove(&i);

            match s.parent {
                Some(ref p) => rem(p, i),
                _ => {}
            }
        }

        rem(self, id(i))
    }
}

impl Fold<Function> for Const<'_> {
    fn fold(&mut self, f: Function) -> Function {
        self.child(|c| f.fold_children(c))
    }
}

impl Fold<AssignExpr> for Const<'_> {
    fn fold(&mut self, e: AssignExpr) -> AssignExpr {
        let e = e.fold_children(self);

        match e.left {
            PatOrExpr::Pat(box Pat::Ident(ref i)) => match *e.right {
                Expr::Lit(..) => {
                    self.scope.idents.get_mut().insert(id(i), *e.right.clone());
                }
                _ => self.scope.remove(i),
            },
            _ => {}
        }

        e
    }
}

impl Fold<VarDecl> for Const<'_> {
    fn fold(&mut self, v: VarDecl) -> VarDecl {
        let v = v.fold_children(self);

        for decl in &v.decls {
            match decl.name {
                Pat::Ident(ref i) => match decl.init {
                    Some(ref e @ box Expr::Lit(..)) => {
                        self.scope.idents.get_mut().insert(id(i), *e.clone());
                    }
                    _ => self.scope.remove(i),
                },
                _ => {}
            }
        }

        v
    }
}

impl Fold<Expr> for Const<'_> {
    fn fold(&mut self, e: Expr) -> Expr {
        let e = e.fold_children(self);

        match e {
            Expr::Ident(ref i) => {
                if let Some(e) = self.scope.find(i) {
                    return e;
                }
            }
            Expr::Unary(UnaryExpr {
                span,
                op: op!("typeof"),
                arg,
            }) => {
                // Folds 'typeof(foo)' if foo is a literal, e.g.
                // typeof("bar") --> "string"
                // typeof(6) --> "number"
                if is_literal(&arg) {
                    //
                    if let Some(value) = match *arg {
                        Expr::Fn(..) => Some(js_word!("function")),
                        Expr::Lit(Lit::Str(..)) => Some(js_word!("string")),
                        Expr::Lit(Lit::Num(..)) => Some(js_word!("number")),
                        Expr::Lit(Lit::Bool(..)) => Some(js_word!("boolean")),
                        Expr::Lit(Lit::Null(..)) | Expr::Object(..) | Expr::Array(..) => {
                            Some(js_word!("object"))
                        }

                        Expr::Ident(Ident {
                            sym: js_word!("undefined"),
                            ..
                        }) => Some(js_word!("undefined")),
                        _ => None,
                    } {
                        return Expr::Lit(Lit::Str(Str {
                            span,
                            value,
                            has_escape: false,
                        }));
                    }
                }

                return Expr::Unary(UnaryExpr {
                    span,
                    op: op!("typeof"),
                    arg,
                });
            }

            Expr::Unary(UnaryExpr {
                span,
                op: op!("!"),
                arg,
            }) => {
                let v = arg.as_pure_bool();

                if let Known(v) = v {
                    return Expr::Lit(Lit::Bool(Bool { span, value: !v }));
                }

                return Expr::Unary(UnaryExpr {
                    span,
                    op: op!("!"),
                    arg,
                });
            }

            Expr::Unary(UnaryExpr {
                span,
                op: op!(unary, "-"),
                arg,
            }) => {
                if arg.is_ident_ref_to(js_word!("Infinity")) {
                    return Expr::Unary(UnaryExpr {
                        span,
                        op: op!(unary, "-"),
                        arg,
                    });
                }

                // "-NaN" is "NaN".
                if arg.is_nan() {
                    return Expr::Lit(Lit::Num(Number { span, value: NAN }));
                }

                if let Known(value) = arg.as_number() {
                    return Expr::Lit(Lit::Num(Number {
                        span,
                        value: -value,
                    }));
                }

                return Expr::Unary(UnaryExpr {
                    span,
                    op: op!(unary, "-"),
                    arg,
                });
            }

            Expr::Unary(UnaryExpr {
                span,
                op: op!("~"),
                arg,
            }) => {
                if let Known(value) = arg.as_number() {
                    if value.fract() == 0.0 {
                        return Expr::Lit(Lit::Num(Number {
                            span,
                            value: !(value as i64) as f64,
                        }));
                    }
                    // TODO: Report error
                }

                return Expr::Unary(UnaryExpr {
                    span,
                    op: op!("~"),
                    arg,
                });
            }

            Expr::Bin(BinExpr {
                span,
                left,
                op,
                right,
            }) => {
                match op {
                    op!("instanceof") if is_literal(&left) && !right.may_have_side_effects() => {
                        // Non-object types are never instances.
                        if left.is_immutable_value() {
                            return Expr::Lit(Lit::Bool(Bool { span, value: false }));
                        }

                        if right.is_ident_ref_to(js_word!("Object")) {
                            return Expr::Lit(Lit::Bool(Bool { span, value: false }));
                        }
                    }

                    _ => {}
                }

                return Expr::Bin(BinExpr {
                    span,
                    left,
                    op,
                    right,
                });
            }

            _ => {}
        }

        e
    }
}

impl Fold<UnaryExpr> for Const<'_> {
    fn fold(&mut self, e: UnaryExpr) -> UnaryExpr {
        let e = e.fold_children(self);

        match e.op {
            op!("void") if !e.arg.may_have_side_effects() => {
                return UnaryExpr {
                    arg: box Expr::Lit(Lit::Num(Number {
                        span: e.arg.span(),
                        value: 0.0,
                    })),
                    ..e
                }
            }

            _ => {}
        }

        e
    }
}
