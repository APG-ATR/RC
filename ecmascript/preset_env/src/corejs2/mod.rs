use self::data::*;
use swc_atoms::{js_word, JsWord};
use swc_common::{Visit, VisitWith};
use swc_ecma_ast::*;

mod data;

pub(crate) struct UsageVisitor {
    pub required: Vec<JsWord>,
}

impl UsageVisitor {
    /// Add imports
    fn add(&mut self, features: &[&str]) {
        self.required.extend(
            features
                .iter()
                .map(|v| format!("core-js/modules/{}", v))
                .map(From::from),
        );
    }
}

// TODO:
//     Program(path: NodePath) {
//      path.get("body").forEach(bodyPath => {
//        if (isPolyfillSource(getRequireSource(bodyPath))) {
//          console.warn(NO_DIRECT_POLYFILL_IMPORT);
//          bodyPath.remove();
//        }
//      });
//    },

/// Detects usage of types
impl Visit<Ident> for UsageVisitor {
    fn visit(&mut self, node: &Ident) {
        node.visit_children(self);

        for (name, builtin) in BUILTIN_TYPES {
            if node.sym == **name {
                self.add(builtin)
            }
        }
    }
}

/// Detects usage of instance properties and static properties.
impl Visit<MemberExpr> for UsageVisitor {
    fn visit(&mut self, node: &MemberExpr) {
        node.visit_children(self);

        match *node.prop {
            Expr::Ident(ref i) => {
                //
                for (name, imports) in INSTANCE_PROPERTIES {
                    if i.sym == **name {
                        self.add(imports)
                    }
                }
            }
            _ => {}
        }

        match node.obj {
            ExprOrSuper::Expr(box Expr::Ident(ref obj)) => {
                for (ty, props) in STATIC_PROPERTIES {
                    if obj.sym == **ty {
                        match *node.prop {
                            Expr::Ident(ref p) => {
                                for (prop, imports) in *props {
                                    if p.sym == **prop {
                                        self.add(imports);
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

/// `arr[Symbol.iterator]()`
impl Visit<CallExpr> for UsageVisitor {
    fn visit(&mut self, e: &CallExpr) {
        e.visit_children(self);

        if !e.args.is_empty()
            && match e.callee {
                ExprOrSuper::Expr(box Expr::Member(MemberExpr { computed: true, .. })) => false,
                _ => true,
            }
            && match e.args[0] {
                ExprOrSpread {
                    expr:
                        box Expr::Member(MemberExpr {
                            obj:
                                ExprOrSuper::Expr(box Expr::Ident(Ident {
                                    sym: js_word!("Symbol"),
                                    ..
                                })),
                            prop:
                                box Expr::Ident(Ident {
                                    sym: js_word!("iterator"),
                                    ..
                                }),
                            computed: false,
                            ..
                        }),
                    ..
                } => true,
                _ => false,
            }
        {
            self.add(&["web.dom.iterable"])
        }
    }
}
