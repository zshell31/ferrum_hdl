use darling::{FromAttributes, FromDeriveInput, FromGenerics};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Generics, Ident};

use crate::utils::{self, Bounds, TEither};

pub struct SignalValueDerive {
    ident: Ident,
    generics: Generics,
    attrs: Bounds,
}

impl FromDeriveInput for SignalValueDerive {
    fn from_derive_input(input: &syn::DeriveInput) -> darling::Result<Self> {
        let attrs = input
            .attrs
            .iter()
            .filter_map(|attr| {
                let ident = attr.path();
                if ident.is_ident("signal_value") {
                    Some(attr.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        Ok(Self {
            ident: input.ident.clone(),
            generics: Generics::from_generics(&input.generics)?,
            attrs: Bounds::from_attributes(&attrs)?,
        })
    }
}

impl SignalValueDerive {
    pub fn into_tokens(self) -> TokenStream {
        let ident = &self.ident;

        let (impl_generics, ty_generics, predicates) =
            utils::split_generics_for_impl(&self.generics);

        let predicates = predicates
            .chain(
                self.generics
                    .type_params()
                    .filter(|type_param| !self.attrs.contains_ty_param(type_param))
                    .map(|type_param| {
                        let ident = &type_param.ident;

                        TEither::TS(quote! {
                            #ident: ::ferrum_hdl::signal::SignalValue
                        })
                    }),
            )
            .chain(self.attrs.0.iter().map(TEither::AsIs));

        let where_clause = utils::into_where_clause(predicates);

        quote! {
            #[automatically_derived]
            impl #impl_generics ::ferrum_hdl::signal::SignalValue for #ident #ty_generics
            #where_clause
            {}
        }
    }
}
