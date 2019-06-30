use super::Analyzer;
use crate::{
    errors::Error,
    ty::{Conditional, Mapped, Tuple, Type, TypeParam, TypeParamDecl, TypeRefExt},
    util::IntoCow,
};
use std::borrow::Cow;
use swc_common::Spanned;
use swc_ecma_ast::*;

impl Analyzer<'_, '_> {
    pub(super) fn expand_type_params<'a, 'b>(
        &self,
        i: Option<&TsTypeParamInstantiation>,
        decl: &TypeParamDecl,
        mut ty: Cow<'b, Type<'a>>,
    ) -> Result<Cow<'b, Type<'a>>, Error> {
        match *ty.normalize() {
            Type::TypeLit(..) | Type::Keyword(..) | Type::Lit(..) => return Ok(ty),

            Type::Simple(ref sty) => match **sty {
                TsType::TsTypeRef(TsTypeRef {
                    type_name: TsEntityName::Ident(Ident { ref sym, .. }),
                    ..
                }) => {
                    // Handle references to type parameters
                    for (idx, p) in decl.params.iter().enumerate() {
                        if p.name == *sym {
                            if let Some(i) = i {
                                return Ok(Type::from(i.params[idx].clone()).owned());
                            }

                            if let Some(ref default) = p.default {
                                return Ok(default.to_static().owned());
                            }

                            unimplemented!("expand_type_param: type inference")
                        }
                    }

                    // Normal type reference
                    return Ok(ty);
                }

                _ => {}
            },

            Type::Operator(ref op) => {
                let expanded = self.expand_type_params(i, decl, Cow::Borrowed(&op.ty))?;

                if let Cow::Owned(expanded) = expanded {
                    match ty.to_mut() {
                        Type::Operator(ref mut operator) => {
                            operator.ty = box Cow::Owned(expanded);
                        }
                        Type::Static(..) => unimplemented!("Type::Static(Type::Operator(..))"),

                        ref ty => unreachable!("{:#?}", ty),
                    }
                }

                return Ok(ty);
            }

            Type::Conditional(Conditional {
                ref check_type,
                ref extends_type,
                ref true_type,
                ref false_type,
                ..
            }) => {
                let check_type = self.expand_type_params(i, decl, Cow::Borrowed(check_type))?;
                if let Some(v) = self.extends(&check_type, &extends_type) {
                    return Ok(if v {
                        *true_type.clone()
                    } else {
                        *false_type.clone()
                    });
                }

                //
                unimplemented!(
                    "expanding conditional type.\nParams: {:#?}\nParam decls: {:#?}\nCheck Type: \
                     {:#?}\nextends_type: {:#?}\nTrue type: {:#?}\nFalse type: {:#?}",
                    i,
                    decl,
                    check_type,
                    extends_type,
                    true_type,
                    false_type
                )
            }

            Type::Mapped(Mapped {
                readonly,
                optional,
                ty: ref type_ann,
                ref type_param,
                ..
            }) => {
                let type_param = self.expand_type_param(i, decl, type_param)?;

                // TODO:
                //
                //     type T50<T> = { [P in keyof T]: number };
                //     type T51 = T50<any>;  // { [x: string]: number }
                //     type T52 = T50<unknown>;  // {}

                if let Cow::Owned(type_param) = type_param {
                    match ty.to_mut() {
                        Type::Mapped(ref mut ty) => {
                            ty.type_param = type_param;
                        }

                        Type::Static(..) => unimplemented!("Type::Static(Type::Mapped(..))"),

                        ref ty => unreachable!("{:#?}", ty),
                    }
                }

                return Ok(ty);
            }

            Type::Tuple(Tuple { ref types, .. }) => {
                let mut buf = vec![];

                for (idx, t) in types.into_iter().enumerate() {
                    let t = self.expand_type_params(i, decl, Cow::Borrowed(&t))?;
                    if let Cow::Owned(t) = t {
                        buf.push((idx, t))
                    }
                }

                for (idx, t) in buf {
                    match ty.to_mut() {
                        Type::Tuple(ref mut tuple) => {
                            tuple.types[idx] = Cow::Owned(t);
                        }

                        Type::Static(..) => unimplemented!("Type::Static(Type::Tuple(..))"),

                        _ => unreachable!(),
                    }
                }

                return Ok(ty);
            }
            _ => {}
        }

        unimplemented!(
            "expand_type_params({:#?})\nDecl: {:#?}\nInstantiation: {:#?}",
            ty,
            decl,
            i
        )
    }

    fn expand_type_param<'a, 'b>(
        &self,
        i: Option<&TsTypeParamInstantiation>,
        decl: &TypeParamDecl,
        type_param: &'a TypeParam<'b>,
    ) -> Result<Cow<'a, TypeParam<'b>>, Error> {
        let mut tp = Cow::Borrowed(type_param);

        if let Some(ref c) = type_param.constraint {
            let c = self.expand_type_params(i, decl, Cow::Borrowed(c))?;

            if let Cow::Owned(c) = c {
                tp.to_mut().constraint = Some(box c.into_cow());
            }
        }

        Ok(tp)
    }

    /// Returns `Some(true)` if `child` extends `parent`.
    fn extends(&self, child: &Type, parent: &Type) -> Option<bool> {
        let span = child.span();
        match child.assign_to(parent, span) {
            Ok(()) => return Some(true),
            _ => return Some(false),
        }
    }
}