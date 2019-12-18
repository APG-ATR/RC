use crate::{
    pass::Pass,
    util::{StmtLike, *},
};
use ast::*;
use fxhash::FxHashMap;
use swc_common::{fold::VisitWith, util::move_map::MoveMap, Fold, FoldWith, Spanned, DUMMY_SP};

#[cfg(test)]
mod tests;

/// Ported from `PeepholeRemoveDeadCode` of google closure compiler.
pub fn dce() -> impl Pass + 'static {
    Remover::default()
}

#[derive(Debug, Default)]
struct Remover<'a> {
    scope: Scope<'a>,
    top_level: bool,
}

#[derive(Debug, Default)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    vars: FxHashMap<Id, VarInfo>,
}

#[derive(Debug, Default)]
struct VarInfo {
    /// Count of usage.
    cnt: usize,
}

impl<T: StmtLike> Fold<Vec<T>> for Remover<'_>
where
    Self: Fold<T>,
{
    fn fold(&mut self, stmts: Vec<T>) -> Vec<T> {
        let top_level = self.top_level;
        self.top_level = false;

        let mut buf = Vec::with_capacity(stmts.len());

        for stmt_like in stmts {
            let stmt_like = self.fold(stmt_like);
            let stmt_like = match stmt_like.try_into_stmt() {
                Ok(stmt) => {
                    let stmt = match stmt {
                        // Remove empty statements.
                        Stmt::Empty(..) => continue,

                        // Control flow
                        Stmt::Throw(..)
                        | Stmt::Return { .. }
                        | Stmt::Continue { .. }
                        | Stmt::Break { .. } => {
                            let stmt_like = T::from_stmt(stmt);
                            buf.push(stmt_like);
                            return buf;
                        }

                        Stmt::Block(BlockStmt { stmts, .. }) => {
                            buf.extend(stmts.into_iter().map(T::from_stmt));
                            continue;
                        }

                        // Optimize if statement.
                        Stmt::If(IfStmt {
                            test,
                            cons,
                            alt,
                            span,
                        }) => {
                            // check if
                            let node = match test.as_bool() {
                                (Pure, Known(val)) => {
                                    if val {
                                        *cons
                                    } else {
                                        alt.map(|e| *e).unwrap_or(Stmt::Empty(EmptyStmt { span }))
                                    }
                                }
                                // TODO: Impure
                                _ => Stmt::If(IfStmt {
                                    test,
                                    cons,
                                    alt,
                                    span,
                                }),
                            };
                            node
                        }

                        Stmt::Decl(Decl::Var(var)) => {
                            let mut idents = vec![];
                            let mut v = DestructuringFinder { found: &mut idents };
                            var.visit_with(&mut v);

                            self.scope.vars.extend(
                                idents
                                    .into_iter()
                                    .map(|(sym, span)| ((sym, span.ctxt()), VarInfo::default())),
                            );

                            Stmt::Decl(Decl::Var(var))
                        }

                        _ => stmt,
                    };

                    T::from_stmt(stmt)
                }
                Err(stmt_like) => stmt_like,
            };

            buf.push(stmt_like);
        }

        buf
    }
}

impl Fold<Stmt> for Remover<'_> {
    fn fold(&mut self, stmt: Stmt) -> Stmt {
        let stmt = stmt.fold_children(self);

        match stmt {
            Stmt::If(IfStmt {
                span,
                test,
                cons: box Stmt::Empty(..),
                alt: None,
            }) => {
                let expr = ignore_result(*test).map(Box::new);
                match expr {
                    Some(expr) => Stmt::Expr(ExprStmt { span, expr }),
                    None => Stmt::Empty(EmptyStmt { span }),
                }
            }

            Stmt::Labeled(LabeledStmt {
                span,
                body: box Stmt::Empty(..),
                ..
            }) => Stmt::Empty(EmptyStmt { span }),

            // `1;` -> `;`
            Stmt::Expr(ExprStmt {
                span,
                expr: box expr,
                ..
            }) => match ignore_result(expr) {
                Some(e) => Stmt::Expr(ExprStmt { span, expr: box e }),
                None => Stmt::Empty(EmptyStmt { span: DUMMY_SP }),
            },

            Stmt::Block(BlockStmt { span, stmts }) => {
                if stmts.is_empty() {
                    Stmt::Empty(EmptyStmt { span })
                } else if stmts.len() == 1 {
                    // TODO: Check if lexical variable exists.
                    stmts.into_iter().next().unwrap()
                } else {
                    Stmt::Block(BlockStmt { span, stmts })
                }
            }

            Stmt::Try(TryStmt {
                span,
                block,
                handler,
                finalizer,
            }) => {
                // Only leave the finally block if try block is empty
                if block.is_empty() {
                    return finalizer
                        .map(Stmt::Block)
                        .unwrap_or(Stmt::Empty(EmptyStmt { span }));
                }

                // If catch block and finally block is empty, remove try-catch is useless.
                if handler.is_empty() && finalizer.is_empty() {
                    return Stmt::Block(block);
                }

                Stmt::Try(TryStmt {
                    span,
                    block,
                    handler,
                    finalizer,
                })
            }

            // Remove empty else block.
            // As we fold children before parent, unused expression
            // statements without side effects are converted to
            // Stmt::Empty before here.
            Stmt::If(IfStmt {
                span,
                test,
                cons,
                alt,
            }) => {
                if alt.is_empty() {
                    return Stmt::If(IfStmt {
                        span,
                        test,
                        cons,
                        alt: None,
                    });
                }
                Stmt::If(IfStmt {
                    span,
                    test,
                    cons,
                    alt,
                })
            }

            Stmt::Switch(mut s) => {
                if s.cases.is_empty() {
                    match ignore_result(*s.discriminant) {
                        Some(expr) => {
                            return Stmt::Expr(ExprStmt {
                                span: s.span,
                                expr: box expr,
                            })
                        }
                        None => return Stmt::Empty(EmptyStmt { span: s.span }),
                    }
                }

                let selected = s.cases.iter().position(|case| {
                    if let Some(ref test) = case.test {
                        return match (&**test, &*s.discriminant) {
                            (
                                &Expr::Lit(Lit::Str(Str {
                                    value: ref test, ..
                                })),
                                &Expr::Lit(Lit::Str(Str { value: ref d, .. })),
                            ) => *test == *d,
                            (
                                &Expr::Lit(Lit::Num(Number { value: test, .. })),
                                &Expr::Lit(Lit::Num(Number { value: d, .. })),
                            ) => test == d,
                            (&Expr::Lit(Lit::Null(..)), &Expr::Lit(Lit::Null(..))) => true,
                            (&Expr::Ident(ref test), &Expr::Ident(ref d)) => {
                                test.sym == d.sym && test.span.ctxt() == d.span.ctxt()
                            }
                            _ => false,
                        };
                    }

                    false
                });

                let remove_break = |stmts: &mut Vec<Stmt>| {
                    let mut done = false;
                    stmts.retain(|v| {
                        if done {
                            return false;
                        }
                        match v {
                            Stmt::Break(BreakStmt { label: None, .. }) => {
                                done = true;
                                false
                            }
                            Stmt::Return(..) | Stmt::Throw(..) => {
                                done = true;
                                true
                            }
                            _ => true,
                        }
                    })
                };

                if let Some(i) = selected {
                    let mut stmts = s.cases.remove(i).cons;
                    remove_break(&mut stmts);

                    return Stmt::Block(BlockStmt {
                        span: s.span,
                        stmts,
                    })
                    .fold_with(self);
                } else {
                    match *s.discriminant {
                        Expr::Lit(..) => {
                            let idx = s.cases.iter().position(|v| v.test.is_none());
                            if let Some(i) = idx {
                                let mut stmts = s.cases.remove(i).cons;
                                remove_break(&mut stmts);

                                return Stmt::Block(BlockStmt {
                                    span: s.span,
                                    stmts,
                                })
                                .fold_with(self);
                            }
                        }
                        _ => {}
                    }
                }

                SwitchStmt { ..s }.into()
            }

            _ => stmt,
        }
    }
}

impl Fold<Pat> for Remover<'_> {
    fn fold(&mut self, p: Pat) -> Pat {
        let p = p.fold_children(self);

        match p {
            Pat::Assign(p)
                if p.right.is_undefined()
                    || match *p.right {
                        Expr::Unary(UnaryExpr {
                            op: op!("void"),
                            ref arg,
                            ..
                        }) => is_literal(&arg),
                        _ => false,
                    } =>
            {
                return *p.left;
            }
            _ => {}
        }

        p
    }
}

impl Fold<ObjectPatProp> for Remover<'_> {
    fn fold(&mut self, p: ObjectPatProp) -> ObjectPatProp {
        let p = p.fold_children(self);

        match p {
            ObjectPatProp::Assign(AssignPatProp {
                span,
                key,
                value: Some(expr),
            }) if expr.is_undefined()
                || match *expr {
                    Expr::Unary(UnaryExpr {
                        op: op!("void"),
                        ref arg,
                        ..
                    }) => is_literal(&arg),
                    _ => false,
                } =>
            {
                return ObjectPatProp::Assign(AssignPatProp {
                    span,
                    key,
                    value: None,
                });
            }
            _ => {}
        }

        p
    }
}

impl Fold<SeqExpr> for Remover<'_> {
    fn fold(&mut self, e: SeqExpr) -> SeqExpr {
        let e = e.fold_children(self);

        SeqExpr {
            exprs: e.exprs.move_flat_map(|e| ignore_result(*e).map(Box::new)),
            ..e
        }
    }
}

impl Fold<Expr> for Remover<'_> {
    fn fold(&mut self, e: Expr) -> Expr {
        let e: Expr = e.fold_children(self);

        match e {
            Expr::Assign(AssignExpr {
                span,
                op: op!("="),
                left: PatOrExpr::Pat(box Pat::Ident(ref l)),
                right: box Expr::Ident(ref r),
            }) if l.sym == r.sym && l.span.ctxt() == r.span.ctxt() => return *undefined(span),
            _ => {}
        }

        e
    }
}

impl Fold<ForStmt> for Remover<'_> {
    fn fold(&mut self, s: ForStmt) -> ForStmt {
        let s = s.fold_children(self);

        ForStmt {
            init: s.init.and_then(|e| match e {
                VarDeclOrExpr::Expr(e) => ignore_result(*e).map(Box::new).map(VarDeclOrExpr::from),
                _ => Some(e),
            }),
            update: s.update.and_then(|e| ignore_result(*e).map(Box::new)),
            test: s.test.and_then(|e| ignore_result(*e).map(Box::new)),
            ..s
        }
    }
}

/// Ignores the result.
///
/// Returns
///  - [Some] if `e` has a side effect.
///  - [None] if `e` does not have a side effect.
#[inline(never)]
fn ignore_result(e: Expr) -> Option<Expr> {
    match e {
        Expr::Lit(Lit::Num(..))
        | Expr::Lit(Lit::Bool(..))
        | Expr::Lit(Lit::Regex(..))
        | Expr::Ident(..) => None,

        Expr::Paren(ParenExpr { expr, .. }) => ignore_result(*expr),

        Expr::Bin(BinExpr {
            span,
            left,
            op,
            right,
        }) if op != op!("&&") && op != op!("||") => {
            let left = ignore_result(*left);
            let right = ignore_result(*right);

            match (left, right) {
                (Some(l), Some(r)) => {
                    ignore_result(preserve_effects(span, *undefined(span), vec![box l, box r]))
                }
                (Some(l), None) => Some(l),
                (None, Some(r)) => Some(r),
                (None, None) => None,
            }
        }

        Expr::Unary(UnaryExpr { span, op, arg }) => match op {
            op!("void")
            | op!("typeof")
            | op!(unary, "+")
            | op!(unary, "-")
            | op!("!")
            | op!("~") => ignore_result(*arg),
            _ => Some(Expr::Unary(UnaryExpr { span, op, arg })),
        },

        Expr::Array(ArrayLit { span, elems, .. }) => {
            let mut has_spread = false;
            let elems = elems.move_flat_map(|v| match v {
                Some(ExprOrSpread {
                    spread: Some(..), ..
                }) => {
                    has_spread = true;
                    Some(v)
                }
                None => None,
                Some(ExprOrSpread { spread: None, expr }) => ignore_result(*expr).map(|expr| {
                    Some(ExprOrSpread {
                        spread: None,
                        expr: box expr,
                    })
                }),
            });

            if elems.is_empty() {
                None
            } else {
                if has_spread {
                    Some(Expr::Array(ArrayLit { span, elems }))
                } else {
                    ignore_result(preserve_effects(
                        span,
                        *undefined(span),
                        elems.into_iter().map(|v| v.unwrap().expr),
                    ))
                }
            }
        }

        Expr::Object(ObjectLit { span, props, .. }) => {
            let props = props.move_flat_map(|v| match v {
                PropOrSpread::Spread(..) => Some(v),
                PropOrSpread::Prop(ref p) => {
                    if is_literal(&p) {
                        None
                    } else {
                        Some(v)
                    }
                }
            });

            if props.is_empty() {
                None
            } else {
                Some(Expr::Object(ObjectLit { span, props }))
            }
        }

        Expr::Call(CallExpr {
            span,
            callee: ExprOrSuper::Expr(ref callee),
            args,
            ..
        }) if callee.is_pure_callee() => ignore_result(Expr::Array(ArrayLit {
            span,
            elems: args.into_iter().map(Some).collect(),
        })),

        Expr::TaggedTpl(TaggedTpl {
            span, tag, exprs, ..
        }) if tag.is_pure_callee() => {
            ignore_result(preserve_effects(span, *undefined(span), exprs))
        }

        //
        // Function expressions are useless if they are not used.
        //
        // As function expressions cannot start with 'function',
        // this will be reached only if other things
        // are removed while folding children.
        Expr::Fn(..) => None,

        Expr::Seq(SeqExpr {
            span, mut exprs, ..
        }) => {
            if exprs.is_empty() {
                return None;
            }

            let last = ignore_result(*exprs.pop().unwrap()).map(Box::new);

            exprs.extend(last);

            Some(Expr::Seq(SeqExpr { span, exprs }))
        }

        Expr::Cond(CondExpr {
            span,
            test,
            cons,
            alt,
        }) => {
            let cons_span = cons.span();
            let alt_span = alt.span();
            Some(Expr::Cond(CondExpr {
                span,
                test,
                cons: ignore_result(*cons)
                    .map(Box::new)
                    .unwrap_or_else(|| undefined(cons_span)),
                alt: ignore_result(*alt)
                    .map(Box::new)
                    .unwrap_or_else(|| undefined(alt_span)),
            }))
        }

        _ => Some(e),
    }
}
