use std::{
    fmt::{self, Display},
    iter,
    ops::Deref,
};

use darling::{
    ast::{Data, Fields, NestedMeta},
    FromField, FromMeta, FromVariant,
};
use either::Either;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse::Parser, punctuated::Punctuated, spanned::Spanned, token::Comma, Expr, ExprLit,
    ExprParen, ExprTuple, Generics, Ident, ImplGenerics, Lit, Meta, PredicateType, Type,
    TypeArray, TypeGenerics, TypeParam, TypePath, TypeTuple, WherePredicate,
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

pub trait MetaExt: FromMeta + Sized {
    fn from_tuple(tuple: &ExprTuple) -> darling::Result<Self> {
        let parser = Punctuated::<NestedMeta, Comma>::parse_terminated;
        let items = parser
            .parse(tuple.elems.to_token_stream().into())?
            .into_iter()
            .collect::<Vec<_>>();

        Self::from_list(&items).map_err(|e| e.with_span(&tuple.span()))
    }

    fn from_paren(paren: &ExprParen) -> darling::Result<Self> {
        let parser = Punctuated::<NestedMeta, Comma>::parse_terminated;
        let items = parser
            .parse(paren.expr.to_token_stream().into())?
            .into_iter()
            .collect::<Vec<_>>();

        Self::from_list(&items)
    }
}

impl<T: FromMeta + Sized> MetaExt for T {}

#[derive(Debug, Clone, Copy)]
pub struct SpannedValue<T> {
    value: T,
    span: Span,
}

impl<T> SpannedValue<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn into_inner(self) -> T {
        self.value
    }
}

impl<T: Display> Display for SpannedValue<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: Default> Default for SpannedValue<T> {
    fn default() -> Self {
        Self::new(Default::default(), Span::call_site())
    }
}

impl<T> Deref for SpannedValue<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: FromMeta> FromMeta for SpannedValue<T> {
    fn from_meta(item: &Meta) -> darling::Result<Self> {
        let value = T::from_meta(item).map_err(|e| e.with_span(item))?;

        let span = match item {
            Meta::Path(path) => path.span(),
            Meta::List(list) => list.tokens.span(),
            Meta::NameValue(nv) => nv.value.span(),
        };

        Ok(Self::new(value, span))
    }
}
