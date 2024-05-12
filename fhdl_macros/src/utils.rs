use std::iter;

use darling::{
    ast::{Data, Fields},
    FromField, FromMeta, FromVariant,
};
use either::Either;
use proc_macro2::{Span, TokenStream};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{quote, ToTokens};
use syn::{
    punctuated::Punctuated, token::Comma, Expr, ExprLit, Generics, Ident, ImplGenerics,
    Lit, PredicateType, Type, TypeArray, TypeGenerics, TypeParam, TypePath, TypeTuple,
    WherePredicate,
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

#[derive(Debug, Default)]
pub struct Bounds(pub Vec<WherePredicate>);

impl FromMeta for Bounds {
    fn from_value(value: &Lit) -> darling::Result<Self> {
        (if let Lit::Str(value) = value {
            value
                .parse_with(Punctuated::<WherePredicate, Comma>::parse_separated_nonempty)
                .map(|bounds| Self(bounds.into_iter().collect()))
                .map_err(Into::into)
        } else {
            Err(darling::Error::unexpected_lit_type(value))
        })
        .map_err(|e| e.with_span(value))
    }
}

impl Bounds {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn push(&mut self, bounds: Self) {
        self.0.extend(bounds.0)
    }

    pub fn extend_predicates<'a>(
        &'a self,
        predicates: impl Iterator<Item = TEither<&'a WherePredicate>> + 'a,
        generics: &'a Generics,
        exclude_array_constr: bool,
        f: impl Fn(&'a TypeParam) -> TEither<&'a WherePredicate> + 'a,
    ) -> impl Iterator<Item = TEither<&'a WherePredicate>> + 'a {
        predicates
            .chain(
                generics
                    .type_params()
                    .filter(|tparam| !self.contains_ty_param(tparam))
                    .map(f),
            )
            .chain(
                self.0
                    .iter()
                    .filter(move |bound| {
                        if exclude_array_constr {
                            !Self::is_array_constr(bound)
                        } else {
                            true
                        }
                    })
                    .map(TEither::AsIs),
            )
    }

    fn contains_ty_param(&self, tparam: &TypeParam) -> bool {
        self.0.iter().any(|bound| match bound {
            WherePredicate::Type(PredicateType {
                bounded_ty: Type::Path(TypePath { path, .. }),
                ..
            }) => path.is_ident(&tparam.ident),
            _ => false,
        })
    }

    fn is_array_constr(pred: &WherePredicate) -> bool {
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

#[derive(Debug, FromField)]
pub struct Field {
    pub ident: Option<Ident>,
    pub ty: Type,
}

#[derive(Debug, FromVariant)]
pub struct Variant {
    pub ident: Ident,
    pub fields: Fields<Field>,
    pub discriminant: Option<Expr>,
}

impl Variant {
    pub fn discr(&self) -> Option<usize> {
        self.discriminant.as_ref().and_then(|discr| match discr {
            Expr::Lit(ExprLit {
                lit: Lit::Int(lit), ..
            }) => lit.base10_parse().ok(),
            _ => None,
        })
    }

    pub fn branch(&self, idx: &mut usize) -> usize {
        let branch = match self.discr() {
            Some(discr) => {
                *idx = discr;
                discr
            }
            None => *idx,
        };
        *idx += 1;
        branch
    }
}

pub type AdtData = Data<Variant, Field>;

pub fn ferrum_hdl_crate() -> TokenStream {
    let ferrum_hdl = crate_name("ferrum_hdl").expect("ferrum_hdl is not found");

    match ferrum_hdl {
        FoundCrate::Itself => quote! { crate },
        FoundCrate::Name(name) => {
            let ident = Ident::new(&name, Span::call_site());
            quote! { #ident }
        }
    }
}
