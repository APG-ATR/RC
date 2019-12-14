#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

use semver::Version;
use serde::Deserialize;
use swc_atoms::JsWord;
use swc_common::{Fold, Visit, VisitWith, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_transforms::{pass::Pass, util::prepend_stmts};

mod corejs2_data;

pub fn polyfills(mut c: Config) -> impl Pass {
    if c.core_js == 0 {
        c.core_js = 2;
    }

    Polyfills { c }
}

#[derive(Debug, Deserialize, Clone, Copy)]
#[serde(deny_unknown_fields)]
pub struct BrowserData<T> {
    pub chrome: T,
    pub edge: T,
    pub firefox: T,
    pub safari: T,
    pub node: T,
    pub ios: T,
    pub samsung: T,
    pub opera: T,
    pub electron: T,
}

struct Polyfills {
    c: Config,
}

impl Fold<Module> for Polyfills {
    fn fold(&mut self, mut node: Module) -> Module {
        let span = node.span;

        if self.c.mode == Some(Mode::Usage) {
            let mut v = UsageVisitor {
                core_js: self.c.core_js,
                required: vec![],
            };
            node.visit_with(&mut v);

            prepend_stmts(
                &mut node.body,
                v.required.into_iter().map(|src| {
                    ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                        span,
                        specifiers: vec![],
                        src: Str {
                            span: DUMMY_SP,
                            value: src,
                            has_escape: false,
                        },
                    }))
                }),
            );
        }

        node
    }
}

impl Fold<Script> for Polyfills {
    fn fold(&mut self, _: Script) -> Script {
        unimplemented!("automatic polyfill for scripts")
    }
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum Mode {
    #[serde(rename = "usage")]
    Usage,
    #[serde(rename = "entry")]
    Entry,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct Config {
    pub mode: Option<Mode>,
    /// Skipped es features.
    ///
    /// e.g.)
    ///  - `core-js/modules/foo`
    pub skip: Vec<JsWord>,
    /// The version of the used core js.
    pub core_js: usize,

    pub versions: Option<BrowserData<Version>>,
}

struct UsageVisitor {
    core_js: usize,
    required: Vec<JsWord>,
}

/// Detects usage of types
impl Visit<Ident> for UsageVisitor {
    fn visit(&mut self, node: &Ident) {
        node.visit_children(self);

        for (name, builtin) in corejs2_data::BUILTIN_TYPES {
            if node.sym == **name {
                self.required.extend(
                    builtin
                        .into_iter()
                        .map(|v| format!("core-js/modules/{}", v))
                        .map(From::from),
                );
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
                for (name, imports) in corejs2_data::INSTANCE_PROPERTIES {
                    if i.sym == **name {
                        self.required.extend(
                            imports
                                .into_iter()
                                .map(|v| format!("core-js/modules/{}", v))
                                .map(From::from),
                        );
                    }
                }
            }
            _ => {}
        }

        match node.obj {
            ExprOrSuper::Expr(box Expr::Ident(ref obj)) => {
                for (ty, props) in corejs2_data::STATIC_PROPERTIES {
                    if obj.sym == **ty {
                        match *node.prop {
                            Expr::Ident(ref p) => {
                                for (prop, imports) in *props {
                                    if p.sym == **prop {
                                        self.required.extend(
                                            imports
                                                .into_iter()
                                                .map(|v| format!("core-js/modules/{}", v))
                                                .map(From::from),
                                        );
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
