use crate::{
    pass::Pass,
    util::{StmtLike, *},
};
use ast::*;
use fxhash::FxHashMap;
use std::{cmp::min, iter::once};
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
    not_top_level: bool,
    normal_block: bool,
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
        let top_level = !self.not_top_level;
        self.not_top_level = true;
        let is_block_stmt = self.normal_block;
        self.normal_block = false;

        let mut buf = Vec::with_capacity(stmts.len());

        for stmt_like in stmts {
            self.normal_block = true;
            let stmt_like = self.fold(stmt_like);
            self.normal_block = false;

            let stmt_like = match stmt_like.try_into_stmt() {
                Ok(stmt) => {
                    let stmt = match stmt {
                        // Remove empty statements.
                        Stmt::Empty(..) => continue,

                        Stmt::Expr(ExprStmt {
                            expr: box Expr::Lit(..),
                            ..
                        }) if is_block_stmt => continue,

                        // Control flow
                        Stmt::Throw(..)
                        | Stmt::Return { .. }
                        | Stmt::Continue { .. }
                        | Stmt::Break { .. } => {
                            let stmt_like = T::from_stmt(stmt);
                            buf.push(stmt_like);
                            return buf;
                        }

                        Stmt::Block(BlockStmt { span, stmts, .. }) => {
                            if stmts.len() == 0 {
                                continue;
                            }

                            if !is_ok_to_inline_block(&stmts) {
                                BlockStmt {
                                    span,
                                    stmts: stmts.fold_with(self),
                                }
                                .into()
                            } else {
                                buf.extend(stmts.into_iter().map(T::from_stmt));
                                continue;
                            }
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
                                (purity, Known(val)) => {
                                    if !purity.is_pure() {
                                        let expr = ignore_result(*test);

                                        if let Some(expr) = expr {
                                            buf.push(T::from_stmt(Stmt::Expr(ExprStmt {
                                                span: DUMMY_SP,
                                                expr: box expr,
                                            })));
                                        }
                                    }

                                    if val {
                                        // Hoist vars from alt
                                        if let Some(var) =
                                            alt.and_then(|alt| alt.extract_var_ids_as_var())
                                        {
                                            buf.push(T::from_stmt(Stmt::Decl(Decl::Var(var))))
                                        }
                                        *cons
                                    } else {
                                        // Hoist vars from cons
                                        if let Some(var) = cons.extract_var_ids_as_var() {
                                            buf.push(T::from_stmt(Stmt::Decl(Decl::Var(var))))
                                        }
                                        match alt {
                                            Some(alt) => *alt,
                                            None => continue,
                                        }
                                    }
                                }
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

            Stmt::Decl(Decl::Var(v)) if v.decls.is_empty() => {
                Stmt::Empty(EmptyStmt { span: v.span })
            }

            Stmt::Labeled(LabeledStmt {
                span,
                body: box Stmt::Empty(..),
                ..
            }) => Stmt::Empty(EmptyStmt { span }),

            Stmt::Labeled(LabeledStmt {
                span,
                body:
                    box Stmt::Break(BreakStmt {
                        label: Some(ref b), ..
                    }),
                ref label,
                ..
            }) if label.sym == b.sym => Stmt::Empty(EmptyStmt { span }),

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
                } else if stmts.len() == 1 && !is_block_scoped_stuff(&stmts[0]) {
                    stmts.into_iter().next().unwrap().fold_with(self)
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
                    let var = handler.and_then(|h| Stmt::from(h.body).extract_var_ids_as_var());

                    return if let Some(mut finalizer) = finalizer {
                        if let Some(var) = var.map(Decl::from).map(Stmt::from) {
                            prepend(&mut finalizer.stmts, var);
                        }
                        finalizer.into()
                    } else {
                        var.map(Decl::from)
                            .map(Stmt::from)
                            .unwrap_or_else(|| Stmt::Empty(EmptyStmt { span }))
                    };
                }

                // If catch block is not specified and finally block is empty, fold it to simple
                // block.
                if handler.is_none() && finalizer.is_empty() {
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
            }) if alt.is_empty() => {
                return Stmt::If(IfStmt {
                    span,
                    test,
                    cons,
                    alt: None,
                });
            }

            Stmt::Switch(mut s) => {
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

                if s.cases.len() == 1 && s.cases[0].test.is_none() {
                    let mut stmts = s.cases.remove(0).cons;
                    remove_break(&mut stmts);

                    return Stmt::Block(BlockStmt {
                        span: s.span,
                        stmts,
                    })
                    .fold_with(self);
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

            Stmt::For(
                s
                @
                ForStmt {
                    test: Some(box Expr::Lit(Lit::Bool(Bool { value: false, .. }))),
                    ..
                },
            ) => {
                let decl = s.body.extract_var_ids_as_var();
                let body = if let Some(var) = decl {
                    Stmt::Decl(Decl::Var(var))
                } else {
                    Stmt::Empty(EmptyStmt { span: s.span })
                };

                if s.init.is_some() {
                    Stmt::For(ForStmt {
                        body: box body,
                        update: None,
                        ..s
                    })
                } else {
                    body
                }
            }

            Stmt::While(s) => {
                if let (purity, Known(v)) = s.test.as_bool() {
                    if v {
                        Stmt::While(WhileStmt {
                            test: box Expr::Lit(Lit::Bool(Bool {
                                span: s.test.span(),
                                value: true,
                            })),
                            ..s
                        })
                    } else {
                        if purity.is_pure() {
                            Stmt::Empty(EmptyStmt { span: s.span })
                        } else {
                            Stmt::While(WhileStmt {
                                body: box Stmt::Empty(EmptyStmt { span: s.span }),
                                ..s
                            })
                        }
                    }
                } else {
                    Stmt::While(s)
                }
            }

            Stmt::DoWhile(s) => {
                if let Known(v) = s.test.as_pure_bool() {
                    if v {
                        // `for(;;);` is shorter than `do ; while(true);`
                        Stmt::For(ForStmt {
                            span: s.span,
                            init: None,
                            test: None,
                            update: None,
                            body: s.body,
                        })
                    } else {
                        if let Some(test) = ignore_result(*s.test) {
                            BlockStmt {
                                span: s.span,
                                stmts: vec![
                                    prepare_loop_body_for_inlining(*s.body).fold_with(self),
                                    test.into_stmt(),
                                ],
                            }
                            .into()
                        } else {
                            prepare_loop_body_for_inlining(*s.body).fold_with(self)
                        }
                    }
                } else {
                    Stmt::DoWhile(s)
                }
            }

            Stmt::Decl(Decl::Var(v)) => {
                let decls = v.decls.move_flat_map(|v| {
                    if !is_literal(&v.init) {
                        return Some(v);
                    }

                    //
                    match &v.name {
                        Pat::Object(o) if o.props.is_empty() => {
                            return None;
                        }
                        Pat::Array(a) if a.elems.is_empty() => {
                            return None;
                        }

                        _ => Some(v),
                    }
                });

                if decls.is_empty() {
                    return Stmt::Empty(EmptyStmt { span: v.span });
                }

                Stmt::Decl(Decl::Var(VarDecl { decls, ..v }))
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

            Pat::Assign(p)
                if match *p.left {
                    Pat::Object(ref o) => o.props.is_empty(),
                    _ => false,
                } && p.right.is_number() =>
            {
                return *p.left;
            }

            _ => {}
        }

        p
    }
}

impl Fold<ArrayPat> for Remover<'_> {
    fn fold(&mut self, p: ArrayPat) -> ArrayPat {
        let mut p: ArrayPat = p.fold_children(self);

        let mut preserved = None;
        let len = p.elems.len();
        for (i, p) in p.elems.iter().enumerate() {
            let can_be_removed = match p {
                Some(Pat::Array(ref p)) if p.elems.is_empty() => true,
                Some(Pat::Object(ref p)) if p.props.is_empty() => true,
                _ => false,
            };

            if !can_be_removed {
                preserved = Some(min(i + 1, len))
            }
        }

        if let Some(i) = preserved {
            p.elems.drain(i..);
        }

        ArrayPat { ..p }
    }
}

impl Fold<ObjectPat> for Remover<'_> {
    fn fold(&mut self, p: ObjectPat) -> ObjectPat {
        let mut p = p.fold_children(self);

        // Don't remove if there exists a rest pattern
        if p.props.iter().any(|p| match p {
            ObjectPatProp::Rest(..) => true,
            _ => false,
        }) {
            return p;
        }

        fn is_computed(k: &PropName) -> bool {
            match k {
                PropName::Computed(..) => true,
                _ => false,
            }
        }

        p.props.retain(|p| match p {
            ObjectPatProp::KeyValue(KeyValuePatProp {
                key,
                value: box Pat::Object(p),
                ..
            }) if !is_computed(&key) && p.props.is_empty() => false,

            ObjectPatProp::KeyValue(KeyValuePatProp {
                key,
                value: box Pat::Array(p),
                ..
            }) if !is_computed(&key) && p.elems.is_empty() => false,
            _ => true,
        });

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
        let mut e: SeqExpr = e.fold_children(self);
        if e.exprs.is_empty() {
            return e;
        }

        let last = e.exprs.pop().unwrap();
        let mut exprs = e.exprs.move_flat_map(|e| ignore_result(*e).map(Box::new));
        exprs.push(last);

        SeqExpr { exprs, ..e }
    }
}

impl Fold<Expr> for Remover<'_> {
    fn fold(&mut self, e: Expr) -> Expr {
        let e: Expr = e.fold_children(self);

        match e {
            Expr::Assign(AssignExpr {
                op: op!("="),
                left: PatOrExpr::Pat(box Pat::Ident(ref l)),
                right: box Expr::Ident(r),
                ..
            }) if l.sym == r.sym && l.span.ctxt() == r.span.ctxt() => return Expr::Ident(r),

            Expr::Assign(AssignExpr {
                op: op!("="),
                left: PatOrExpr::Pat(box Pat::Array(ref arr)),
                right,
                ..
            }) if arr.elems.is_empty() || arr.elems.iter().all(|v| v.is_none()) => {
                return *right;
            }

            Expr::Assign(AssignExpr {
                op: op!("="),
                left: PatOrExpr::Pat(box Pat::Object(ref obj)),
                right,
                ..
            }) if obj.props.is_empty() => {
                return *right;
            }

            Expr::Cond(e)
                if !e.test.may_have_side_effects()
                    && (e.cons.is_undefined()
                        || match *e.cons {
                            Expr::Unary(UnaryExpr {
                                op: op!("void"),
                                ref arg,
                                ..
                            }) if !arg.may_have_side_effects() => true,
                            _ => false,
                        })
                    && (e.alt.is_undefined()
                        || match *e.alt {
                            Expr::Unary(UnaryExpr {
                                op: op!("void"),
                                ref arg,
                                ..
                            }) if !arg.may_have_side_effects() => true,
                            _ => false,
                        }) =>
            {
                return *e.cons
            }

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
            test: s.test.and_then(|e| {
                let span = e.span();
                if let Known(value) = e.as_pure_bool() {
                    if value {
                        return None;
                    } else {
                        return Some(box Expr::Lit(Lit::Bool(Bool { span, value: false })));
                    }
                }

                Some(e)
            }),
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
        | Expr::Lit(Lit::Null(..))
        | Expr::Lit(Lit::Regex(..))
        | Expr::Ident(..) => None,

        Expr::Lit(Lit::Str(ref v)) if v.value.is_empty() => None,

        Expr::Paren(ParenExpr { expr, .. }) => ignore_result(*expr),

        Expr::Assign(AssignExpr {
            op: op!("="),
            left: PatOrExpr::Pat(box Pat::Ident(ref l)),
            right: box Expr::Ident(r),
            ..
        }) if l.sym == r.sym && l.span.ctxt() == r.span.ctxt() => None,

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

        Expr::Bin(BinExpr {
            span,
            left,
            op,
            right,
        }) => {
            if op == op!("&&") {
                let right = if let Some(right) = ignore_result(*right) {
                    box right
                } else {
                    return ignore_result(*left);
                };

                let l = left.as_pure_bool();

                if let Known(l) = l {
                    Some(Expr::Lit(Lit::Bool(Bool { span, value: l })))
                } else {
                    Some(Expr::Bin(BinExpr {
                        span,
                        left,
                        op,
                        right,
                    }))
                }
            } else {
                debug_assert_eq!(op, op!("||"));

                let l = left.as_pure_bool();

                if let Known(l) = l {
                    if l {
                        None
                    } else {
                        ignore_result(*right)
                    }
                } else {
                    let right = ignore_result(*right);
                    if let Some(right) = right {
                        Some(Expr::Bin(BinExpr {
                            span,
                            left,
                            op,
                            right: box right,
                        }))
                    } else {
                        ignore_result(*left)
                    }
                }
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
                ignore_result(preserve_effects(
                    span,
                    *undefined(DUMMY_SP),
                    once(box Expr::Object(ObjectLit { span, props })),
                ))
            }
        }

        Expr::New(NewExpr {
            span,
            ref callee,
            args,
            ..
        }) if callee.is_pure_callee() => ignore_result(Expr::Array(ArrayLit {
            span,
            elems: args
                .map(|args| args.into_iter().map(Some).collect())
                .unwrap_or_else(Default::default),
        })),

        Expr::Call(CallExpr {
            span,
            callee: ExprOrSuper::Expr(ref callee),
            args,
            ..
        }) if callee.is_pure_callee() => ignore_result(Expr::Array(ArrayLit {
            span,
            elems: args.into_iter().map(Some).collect(),
        })),

        Expr::Tpl(Tpl { span, exprs, .. }) => {
            ignore_result(preserve_effects(span, *undefined(span), exprs))
        }

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
            let alt = if let Some(alt) = ignore_result(*alt) {
                alt
            } else {
                return ignore_result(Expr::Bin(BinExpr {
                    span,
                    left: test,
                    op: op!("&&"),
                    right: cons,
                }));
            };

            let cons = if let Some(cons) = ignore_result(*cons) {
                cons
            } else {
                return ignore_result(Expr::Bin(BinExpr {
                    span,
                    left: test,
                    op: op!("||"),
                    right: box alt,
                }));
            };

            Some(Expr::Cond(CondExpr {
                span,
                test,
                cons: box cons,
                alt: box alt,
            }))
        }

        _ => Some(e),
    }
}

/// # Returns true for
///
/// ```js
/// {
///    var x = 1;
/// }
/// ```
///
/// ```js
/// {
///    var x;
///    var y;
///    var z;
///    {
///        var a;
///        var b;
///    }
/// }
/// ```
///
/// ```js
/// {
///    var a = 0;
///    foo();
/// }
/// ```
///
/// # Returns false for
///
/// ```js
/// a: {
///    break a;
///    var x = 1;
/// }
/// ```
fn is_ok_to_inline_block(s: &[Stmt]) -> bool {
    // TODO: This may be inlinable if return / throw / break / continue exists
    if s.iter().any(|s| is_block_scoped_stuff(s)) {
        return false;
    }

    // variable declared as `var` is hoisted
    let last_var = s.iter().rposition(|s| match s {
        Stmt::Decl(Decl::Var(VarDecl {
            kind: VarDeclKind::Var,
            ..
        })) => true,
        _ => false,
    });

    let last_var = if let Some(pos) = last_var {
        pos
    } else {
        return true;
    };

    let last_stopper = s.iter().rposition(|s| match s {
        Stmt::Return(..) | Stmt::Throw(..) | Stmt::Break(..) | Stmt::Continue(..) => true,
        _ => false,
    });

    if let Some(last_stopper) = last_stopper {
        last_stopper > last_var
    } else {
        true
    }
}

fn is_block_scoped_stuff(s: &Stmt) -> bool {
    match s {
        Stmt::Decl(Decl::Var(VarDecl { kind, .. }))
            if *kind == VarDeclKind::Const || *kind == VarDeclKind::Let =>
        {
            return true;
        }
        Stmt::Decl(Decl::Fn(..)) | Stmt::Decl(Decl::Class(..)) => true,
        _ => false,
    }
}

fn prepare_loop_body_for_inlining(stmt: Stmt) -> Stmt {
    let span = stmt.span();
    let mut stmts = match stmt {
        Stmt::Block(BlockStmt { stmts, .. }) => stmts,
        _ => vec![stmt],
    };

    let mut done = false;
    stmts.retain(|stmt| {
        if done {
            return false;
        }

        match stmt {
            Stmt::Break(BreakStmt { label: None, .. })
            | Stmt::Continue(ContinueStmt { label: None, .. }) => {
                done = true;
                false
            }

            Stmt::Return(..) | Stmt::Throw(..) => {
                done = true;
                true
            }

            _ => true,
        }
    });

    BlockStmt { span, stmts }.into()
}
