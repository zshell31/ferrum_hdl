use std::iter;

use either::Either;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Generics, ImplGenerics, TypeGenerics, WherePredicate};

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
