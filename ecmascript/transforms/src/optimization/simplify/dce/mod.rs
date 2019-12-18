use crate::{
    pass::Pass,
    util::{StmtLike, *},
};
use ast::*;
use fxhash::FxHashMap;
use swc_common::{fold::VisitWith, Fold, FoldWith};

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
            // `1;` -> `;`
            Stmt::Expr(ExprStmt {
                span,
                expr: box node,
                ..
            }) => match node {
                Expr::Lit(Lit::Num(..)) | Expr::Lit(Lit::Bool(..)) | Expr::Lit(Lit::Regex(..)) => {
                    Stmt::Empty(EmptyStmt { span })
                }

                Expr::Array(ArrayLit { ref elems, .. }) if elems.is_empty() => {
                    Stmt::Empty(EmptyStmt { span })
                }

                Expr::Object(ObjectLit { ref props, .. }) if props.is_empty() => {
                    Stmt::Empty(EmptyStmt { span })
                }

                //
                // Function expressions are useless if they are not used.
                //
                // As function expressions cannot start with 'function',
                // this will be reached only if other things
                // are removed while folding chilren.
                Expr::Fn(FnExpr {
                    function: Function { span, .. },
                    ..
                }) => Stmt::Empty(EmptyStmt { span }),
                _ => Stmt::Expr(ExprStmt {
                    span,
                    expr: box node,
                }),
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
