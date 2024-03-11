use darling::{FromDeriveInput, FromGenerics};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Generics, Ident};

use crate::utils::{self, Bounds, TEither};

pub struct SignalValueDerive {
    ident: Ident,
    generics: Generics,
    bounds: Bounds,
}

impl FromDeriveInput for SignalValueDerive {
    fn from_derive_input(input: &syn::DeriveInput) -> darling::Result<Self> {
        Ok(Self {
            ident: input.ident.clone(),
            generics: Generics::from_generics(&input.generics)?,
            bounds: Bounds::from_attrs(&input.attrs, "signal_value")?,
        })
    }
}

impl SignalValueDerive {
    pub fn into_tokens(self) -> TokenStream {
        let ident = &self.ident;

        let (impl_generics, ty_generics, predicates) =
            utils::split_generics_for_impl(&self.generics);

        let predicates =
            self.bounds
                .extend_predicates(predicates, &self.generics, false, |tparam| {
                    let ident = &tparam.ident;

                    TEither::TS(quote! {
                        #ident: ::ferrum_hdl::signal::SignalValue
                    })
                });

        let where_clause = utils::into_where_clause(predicates);

        quote! {
            #[automatically_derived]
            impl #impl_generics ::ferrum_hdl::signal::SignalValue for #ident #ty_generics
            #where_clause
            {}
        }
    }
}
