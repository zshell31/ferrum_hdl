use std::iter;

use darling::FromAttributes;
use either::Either;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    punctuated::Punctuated, token::Comma, Attribute, Expr, ExprLit, Generics,
    ImplGenerics, Lit, Meta, MetaNameValue, PredicateType, Type, TypeArray, TypeGenerics,
    TypeParam, TypePath, TypeTuple, WherePredicate,
};

#[derive(Debug)]
pub enum TEither<T> {
    AsIs(T),
    TS(TokenStream),
}

impl<T: ToTokens> ToTokens for TEither<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::AsIs(t) => t.to_tokens(tokens),
            Self::TS(ts) => ts.to_tokens(tokens),
        }
    }
}

pub fn split_generics_for_impl(
    generics: &Generics,
) -> (
    ImplGenerics<'_>,
    TypeGenerics<'_>,
    impl Iterator<Item = TEither<&'_ WherePredicate>>,
) {
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let where_clause = where_clause
        .map(|where_clause| Either::Left(where_clause.predicates.iter()))
        .unwrap_or_else(|| Either::Right(iter::empty::<&WherePredicate>()))
        .map(TEither::AsIs);

    (impl_generics, ty_generics, where_clause)
}

pub fn into_where_clause<'a>(
    predicates: impl Iterator<Item = TEither<&'a WherePredicate>>,
) -> Option<TokenStream> {
    let mut predicates = predicates.peekable();
    if predicates.peek().is_some() {
        Some(quote! {
            where
                #(#predicates),*
        })
    } else {
        None
    }
}

#[derive(Debug)]
pub struct Bounds(pub Vec<WherePredicate>);

impl Bounds {
    pub fn contains_ty_param(&self, tparam: &TypeParam) -> bool {
        self.0.iter().any(|bound| match bound {
            WherePredicate::Type(PredicateType {
                bounded_ty: Type::Path(TypePath { path, .. }),
                ..
            }) => path.is_ident(&tparam.ident),
            _ => false,
        })
    }

    pub fn is_array_constr(pred: &WherePredicate) -> bool {
        if let WherePredicate::Type(PredicateType {
            bounded_ty: Type::Array(TypeArray { elem, .. }),
            bounds,
            ..
        }) = pred
        {
            if let Type::Tuple(TypeTuple { elems, .. }) = &**elem {
                return elems.is_empty() && bounds.is_empty();
            }
        }

        false
    }
}

impl FromAttributes for Bounds {
    fn from_attributes(attrs: &[Attribute]) -> darling::Result<Self> {
        let mut bounds = vec![];
        for attr in attrs {
            let nested =
                attr.parse_args_with(Punctuated::<Meta, Comma>::parse_terminated)?;

            for meta in nested {
                match meta {
                    Meta::NameValue(MetaNameValue {
                        path,
                        value:
                            Expr::Lit(ExprLit {
                                lit: Lit::Str(value),
                                ..
                            }),
                        ..
                    }) if path.is_ident("bound") => {
                        let predicates = value.parse_with(
                            Punctuated::<WherePredicate, Comma>::parse_separated_nonempty,
                        )?;
                        bounds.extend(predicates);
                    }
                    _ => {}
                }
            }
        }
        Ok(Bounds(bounds))
    }
}
