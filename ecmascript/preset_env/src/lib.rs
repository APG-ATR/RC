#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

use crate::transform_data::FEATURES;
use arrayvec::ArrayVec;
use semver::Version;
use serde::Deserialize;
use swc_atoms::JsWord;
use swc_common::{chain, Fold, Visit, VisitWith, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_transforms::{
    compat::{es2015, es2016, es2017, es2018, es3},
    pass::{noop, Optional, Pass},
    util::prepend_stmts,
};

mod corejs2_data;
mod transform_data;

pub fn preset_env(mut c: Config) -> impl Pass {
    if c.core_js == 0 {
        c.core_js = 2;
    }
    let loose = c.loose;

    let pass = noop();
    macro_rules! add {
        ($prev:expr, $feature:ident, $pass:expr) => {{
            let f = transform_data::Feature::$feature;
            let enable = f.should_enable(&c.versions);
            chain!($prev, Optional::new($pass, enable))
        }};
    }

    // ES2018
    let pass = add!(pass, ObjectRestSpread, es2018::object_rest_spread());
    let pass = add!(pass, OptionalCatchBinding, es2018::optional_catch_binding());

    // ES2017
    let pass = add!(pass, AsyncToGenerator, es2017::async_to_generator());

    // ES2016
    let pass = add!(pass, ExponentiationOperator, es2016::exponentation());

    // ES2015
    let pass = add!(pass, BlockScopedFunctions, es2015::BlockScopedFns);
    let pass = add!(pass, TemplateLiterals, es2015::TemplateLiteral::default());
    let pass = add!(pass, Classes, es2015::Classes::default());
    let pass = add!(
        pass,
        Spread,
        es2015::spread(es2015::spread::Config { loose })
    );
    let pass = add!(pass, FunctionName, es2015::function_name());
    let pass = add!(pass, ArrowFunctions, es2015::arrow());
    let pass = add!(pass, DuplicateKeys, es2015::duplicate_keys());
    let pass = add!(pass, StickyRegex, es2015::StickyRegex);
    // TODO:    InstanceOf,
    let pass = add!(pass, TypeOfSymbol, es2015::TypeOfSymbol);
    let pass = add!(pass, ShorthandProperties, es2015::Shorthand);
    let pass = add!(pass, Parameters, es2015::parameters());
    let pass = add!(
        pass,
        ForOf,
        es2015::for_of(es2015::for_of::Config {
            assume_array: loose
        })
    );
    let pass = add!(pass, ComputedProperties, es2015::computed_properties());
    let pass = add!(
        pass,
        Destructuring,
        es2015::destructuring(es2015::destructuring::Config { loose })
    );
    let pass = add!(pass, BlockScoping, es2015::block_scoping());

    // TODO:
    //    Literals,
    //    ObjectSuper,
    //    DotAllRegex,
    //    UnicodeRegex,
    //    NewTarget,
    //    Regenerator,
    //    AsyncGeneratorFunctions,
    //    UnicodePropertyRegex,
    //    JsonStrings,
    //    NamedCapturingGroupsRegex,

    // ES 3
    let pass = add!(pass, PropertyLiterals, es3::PropertyLiteral);
    let pass = add!(pass, MemberExpressionLiterals, es3::MemberExprLit);
    let pass = add!(
        pass,
        ReservedWords,
        es3::ReservedWord {
            preserve_import: c.dynamic_import
        }
    );

    chain!(pass, Polyfills { c })
}

/// A map without allocation.
#[derive(Debug, Default, Deserialize, Clone, Copy)]
#[serde(deny_unknown_fields)]
pub struct BrowserData<T: Default> {
    #[serde(default)]
    pub chrome: T,
    #[serde(default)]
    pub ie: T,
    #[serde(default)]
    pub edge: T,
    #[serde(default)]
    pub firefox: T,
    #[serde(default)]
    pub safari: T,
    #[serde(default)]
    pub node: T,
    #[serde(default)]
    pub ios: T,
    #[serde(default)]
    pub samsung: T,
    #[serde(default)]
    pub opera: T,
    #[serde(default)]
    pub android: T,
    #[serde(default)]
    pub electron: T,
    #[serde(default)]
    pub phantom: T,
}

impl<T> BrowserData<T>
where
    T: Default,
{
    pub fn map<N: Default>(self, mut op: impl FnMut(&'static str, T) -> N) -> BrowserData<N> {
        BrowserData {
            chrome: op("chrome", self.chrome),
            ie: op("ie", self.ie),
            edge: op("edge", self.edge),
            firefox: op("firefox", self.firefox),
            safari: op("safari", self.safari),
            node: op("node", self.node),
            ios: op("ios", self.ios),
            samsung: op("samsung", self.samsung),
            opera: op("opera", self.opera),
            android: op("android", self.android),
            electron: op("electron", self.electron),
            phantom: op("phantom", self.phantom),
        }
    }

    #[inline]
    pub fn map_value<N: Default>(self, mut op: impl FnMut(T) -> N) -> BrowserData<N> {
        self.map(|_, v| op(v))
    }

    pub fn iter<'a>(&'a self) -> impl 'a + Iterator<Item = (&'static str, &'a T)> {
        let mut arr: ArrayVec<[_; 12]> = Default::default();

        arr.try_extend_from_slice(&[
            ("chrome", &self.chrome),
            ("ie", &self.ie),
            ("edge", &self.edge),
            ("firefox", &self.firefox),
            ("safari", &self.safari),
            ("node", &self.node),
            ("ios", &self.ios),
            ("samsung", &self.samsung),
            ("opera", &self.opera),
            ("android", &self.android),
            ("electron", &self.electron),
            ("phantom", &self.phantom),
        ])
        .unwrap();

        arr.into_iter()
    }
}

impl<T> BrowserData<Option<T>> {
    pub fn as_ref(&self) -> BrowserData<Option<&T>> {
        BrowserData {
            chrome: self.chrome.as_ref(),
            ie: self.ie.as_ref(),
            edge: self.edge.as_ref(),
            firefox: self.firefox.as_ref(),
            safari: self.safari.as_ref(),
            node: self.node.as_ref(),
            ios: self.ios.as_ref(),
            samsung: self.samsung.as_ref(),
            opera: self.opera.as_ref(),
            android: self.android.as_ref(),
            electron: self.electron.as_ref(),
            phantom: self.phantom.as_ref(),
        }
    }
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
    #[serde(default)]
    pub mode: Option<Mode>,

    #[serde(default)]
    pub debug: bool,

    #[serde(default)]
    pub dynamic_import: bool,

    #[serde(default)]
    pub loose: bool,

    /// Skipped es features.
    ///
    /// e.g.)
    ///  - `core-js/modules/foo`
    #[serde(default)]
    pub skip: Vec<JsWord>,

    /// The version of the used core js.
    #[serde(default)]
    pub core_js: usize,

    #[serde(default)]
    pub versions: BrowserData<Option<Version>>,
}

struct UsageVisitor {
    core_js: usize,
    required: Vec<JsWord>,
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

/// Detects usage of types
impl Visit<Ident> for UsageVisitor {
    fn visit(&mut self, node: &Ident) {
        node.visit_children(self);

        for (name, builtin) in corejs2_data::BUILTIN_TYPES {
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
                for (name, imports) in corejs2_data::INSTANCE_PROPERTIES {
                    if i.sym == **name {
                        self.add(imports)
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
