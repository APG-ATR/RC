use crate::errors::Error;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use swc_common::Spanned;
use swc_ecma_ast::*;

pub(super) trait PatExt {
    fn get_ty(&self) -> Option<&TsType>;
    fn set_ty(&mut self, ty: Option<Box<TsType>>);
}

impl PatExt for Pat {
    fn get_ty(&self) -> Option<&TsType> {
        match *self {
            Pat::Array(ArrayPat { ref type_ann, .. })
            | Pat::Assign(AssignPat { ref type_ann, .. })
            | Pat::Ident(Ident { ref type_ann, .. })
            | Pat::Object(ObjectPat { ref type_ann, .. })
            | Pat::Rest(RestPat { ref type_ann, .. }) => type_ann.as_ref().map(|ty| &*ty.type_ann),

            Pat::Expr(ref pat) => unreachable!("Cannot get type from Pat::Expr\n{:?}", pat),
        }
    }

    fn set_ty(&mut self, ty: Option<Box<TsType>>) {
        match *self {
            Pat::Array(ArrayPat {
                ref mut type_ann, ..
            })
            | Pat::Assign(AssignPat {
                ref mut type_ann, ..
            })
            | Pat::Ident(Ident {
                ref mut type_ann, ..
            })
            | Pat::Object(ObjectPat {
                ref mut type_ann, ..
            })
            | Pat::Rest(RestPat {
                ref mut type_ann, ..
            }) => {
                *type_ann = ty.map(|type_ann| TsTypeAnn {
                    span: type_ann.span(),
                    type_ann,
                })
            }

            Pat::Expr(ref pat) => {
                unreachable!("Cannot set type annottation for expression\n{:?}", pat)
            }
        }
    }
}

pub(super) trait TypeExt {
    /// Returns generalized type if `self` is a literal type.
    fn generalizd_lit(self) -> TsType;
}

pub(super) trait TypeRefExt {
    /// Returns type annotation.
    fn ann(&self) -> Option<&TsType>;

    fn contains_undefined(&self) -> bool {
        match self.ann() {
            None => true,
            Some(ref ty) => match **ty {
                TsType::TsKeywordType(TsKeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                }) => true,

                TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(
                    ref t,
                )) => t.types.par_iter().any(|t| t.contains_undefined()),

                TsType::TsThisType(..) => false,
                _ => false,
            },
        }
    }

    fn assign_to(&self, to: &TsType) -> Option<Error> {
        try_assign(
            to,
            match self.ann() {
                Some(v) => v,
                None => return None,
            },
        )
    }
}

fn try_assign(to: &TsType, rhs: &TsType) -> Option<Error> {
    if let TsType::TsKeywordType(TsKeywordType {
        kind: TsKeywordTypeKind::TsAnyKeyword,
        ..
    }) = *rhs
    {
        return None;
    }

    match *to {
        // let a: any = 'foo'
        TsType::TsKeywordType(TsKeywordType {
            kind: TsKeywordTypeKind::TsAnyKeyword,
            ..
        }) => return None,

        TsType::TsThisType(TsThisType { span }) => return Some(Error::CannotAssingToThis { span }),

        // let a: string | number = 'string';
        TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(
            TsUnionType { ref types, .. },
        )) => {
            let vs = types
                .par_iter()
                .map(|to| try_assign(&to, rhs))
                .collect::<Vec<_>>();
            if vs.iter().any(Option::is_none) {
                return None;
            }
            return Some(Error::UnionError {
                errors: vs.into_iter().map(Option::unwrap).collect(),
            });
        }

        TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsIntersectionType(
            TsIntersectionType { ref types, .. },
        )) => {
            let vs = types
                .par_iter()
                .map(|to| try_assign(&to, rhs))
                .collect::<Vec<_>>();

            for v in vs {
                if let Some(error) = v {
                    return Some(Error::IntersectionError { error: box error });
                }
            }

            return None;
        }

        TsType::TsArrayType(TsArrayType { ref elem_type, .. }) => match rhs {
            TsType::TsArrayType(TsArrayType {
                elem_type: ref rhs_elem_type,
                ..
            }) => {
                return try_assign(elem_type, rhs_elem_type).map(|cause| Error::AssignFailed {
                    left: to.clone(),
                    right: rhs.clone(),
                    cause: Some(box cause),
                })
            }
            _ => {
                return Some(Error::AssignFailed {
                    left: to.clone(),
                    right: rhs.clone(),
                    cause: None,
                })
            }
        },

        TsType::TsKeywordType(TsKeywordType {
            kind: TsKeywordTypeKind::TsStringKeyword,
            ..
        }) => match *rhs {
            TsType::TsKeywordType(TsKeywordType {
                kind: TsKeywordTypeKind::TsStringKeyword,
                ..
            }) => return None,

            TsType::TsLitType(TsLitType {
                lit: TsLit::Str(..),
                ..
            }) => return None,

            _ => {}
        },

        TsType::TsKeywordType(TsKeywordType {
            kind: TsKeywordTypeKind::TsNumberKeyword,
            ..
        }) => match *rhs {
            TsType::TsKeywordType(TsKeywordType {
                kind: TsKeywordTypeKind::TsNumberKeyword,
                ..
            }) => return None,

            TsType::TsLitType(TsLitType {
                lit: TsLit::Number(..),
                ..
            }) => return None,

            _ => {}
        },

        TsType::TsKeywordType(TsKeywordType {
            kind: TsKeywordTypeKind::TsBooleanKeyword,
            ..
        }) => match *rhs {
            TsType::TsKeywordType(TsKeywordType {
                kind: TsKeywordTypeKind::TsBooleanKeyword,
                ..
            }) => return None,

            TsType::TsLitType(TsLitType {
                lit: TsLit::Bool(..),
                ..
            }) => return None,

            _ => {}
        },

        _ => {}
    }

    Some(Error::AssignFailed {
        left: to.clone(),
        right: rhs.clone(),
        cause: None,
    })
}

impl TypeRefExt for TsType {
    fn ann(&self) -> Option<&TsType> {
        Some(self)
    }
}

impl TypeRefExt for Option<TsType> {
    fn ann(&self) -> Option<&TsType> {
        self.as_ref()
    }
}

impl TypeRefExt for Option<&'_ TsType> {
    fn ann(&self) -> Option<&TsType> {
        *self
    }
}