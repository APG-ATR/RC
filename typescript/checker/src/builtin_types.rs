use crate::{
    analyzer::{export::pat_to_ts_fn_param, Analyzer, ImportInfo},
    errors::Error,
    loader::Load,
    ty::{self, Class, ClassMember, Method, Module, Static},
};
use chashmap::CHashMap;
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use std::{
    collections::hash_map::Entry,
    path::{Path, PathBuf},
    sync::Arc,
};
use swc_atoms::JsWord;
use swc_common::{FoldWith, Span, VisitWith, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ts_builtin_types::load;
pub use swc_ts_builtin_types::Lib;

type Type = ty::Type<'static>;

#[derive(Debug, Default)]
struct Merged {
    vars: FxHashMap<JsWord, Type>,
    types: FxHashMap<JsWord, Type>,
}

fn merge(ls: &[Lib]) -> &'static Merged {
    lazy_static! {
        static ref CACHE: CHashMap<Vec<Lib>, &'static Merged> = Default::default();
    }

    let mut libs = ls.to_vec();
    if libs.is_empty() {
        libs.push(Lib::Es5);
    }
    let libs = libs;
    if let Some(cached) = CACHE.get(&libs) {
        return &*cached;
    }

    // We hold write lock (thus block readers) while merging.
    CACHE.alter(libs, |v| {
        if let Some(v) = v {
            return Some(v);
        }

        let mut merged = box Merged::default();

        let mut analyzer = Analyzer::for_builtin(&ls, &Noop);

        let modules = load(ls);

        modules.fold_with(&mut analyzer);

        Some(Box::leak(merged))
    });

    return &*CACHE.get(ls).unwrap();
}

pub fn get_var(libs: &[Lib], span: Span, name: &JsWord) -> Result<Type, Error> {
    let lib = merge(libs);

    if let Some(v) = lib.vars.get(&name) {
        return Ok(ty::Type::Static(Static { span, ty: v }));
    }

    Err(Error::NoSuchVar {
        span,
        name: name.clone(),
    })
}

pub fn get_type(libs: &[Lib], span: Span, name: &JsWord) -> Result<Type, Error> {
    let lib = merge(libs);

    if let Some(ty) = lib.types.get(name) {
        return Ok(ty::Type::Static(Static { span, ty }));
    }

    Err(Error::NoSuchType {
        span,
        name: name.clone(),
    })
}

struct Noop;

impl Load for Noop {
    fn load(&self, _: Arc<PathBuf>, _: &ImportInfo) -> Result<FxHashMap<JsWord, Arc<Type>>, Error> {
        unimplemented!()
    }
}
