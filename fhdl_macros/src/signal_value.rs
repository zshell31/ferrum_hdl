use darling::FromDeriveInput;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Generics, Ident};

use crate::utils::{self, ferrum_hdl_crate, Bounds, TEither};

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(signal_value))]
pub struct SignalValue {
    ident: Ident,
    generics: Generics,
    #[darling(default, multiple)]
    bound: Bounds,
}

impl SignalValue {
    pub fn into_tokens(self) -> TokenStream {
        let ident = &self.ident;

        let (impl_generics, ty_generics, predicates) =
            utils::split_generics_for_impl(&self.generics);

        let krate = ferrum_hdl_crate();

        let predicates =
            self.bound
                .extend_predicates(predicates, &self.generics, false, |tparam| {
                    let ident = &tparam.ident;

                    TEither::TS(quote! {
                        #ident: #krate::signal::SignalValue
                    })
                });

        let where_clause = utils::into_where_clause(predicates);

        quote! {
            #[automatically_derived]
            impl #impl_generics #krate::signal::SignalValue for #ident #ty_generics
            #where_clause
            {}
        }
    }
}
