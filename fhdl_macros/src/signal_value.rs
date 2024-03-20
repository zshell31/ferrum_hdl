use darling::FromDeriveInput;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Generics, Ident};

use crate::utils::{self, Bounds, TEither};

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(signal_value))]
pub struct SignalValueDerive {
    ident: Ident,
    generics: Generics,
    #[darling(default, multiple)]
    bound: Bounds,
}

impl SignalValueDerive {
    pub fn into_tokens(self) -> TokenStream {
        let ident = &self.ident;

        let (impl_generics, ty_generics, predicates) =
            utils::split_generics_for_impl(&self.generics);

        let predicates =
            self.bound
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
