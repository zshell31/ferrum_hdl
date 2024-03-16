use std::iter;

use darling::{FromDeriveInput, FromGenerics};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Generics, Ident};

use crate::utils::{self, AdtData, Bounds, TEither};

pub struct TraceableDerive {
    ident: Ident,
    generics: Generics,
    data: AdtData,
    bounds: Bounds,
}

impl FromDeriveInput for TraceableDerive {
    fn from_derive_input(input: &syn::DeriveInput) -> darling::Result<Self> {
        Ok(Self {
            ident: input.ident.clone(),
            generics: Generics::from_generics(&input.generics)?,
            data: AdtData::try_from(&input.data)?,
            bounds: Bounds::from_attrs(&input.attrs, "traceable")?,
        })
    }
}

impl TraceableDerive {
    pub fn into_tokens(self) -> TokenStream {
        let ident = &self.ident;
        let (impl_generics, ty_generics, predicates) =
            utils::split_generics_for_impl(&self.generics);

        let predicates =
            self.bounds
                .extend_predicates(predicates, &self.generics, false, |tparam| {
                    let ident = &tparam.ident;

                    TEither::TS(quote! {
                        #ident: ::ferrum_hdl::trace::Traceable
                    })
                });

        match &self.data {
            AdtData::Enum(_variants) => {
                let predicates = predicates.chain(iter::once(TEither::TS(quote! {
                    Self: ::std::clone::Clone
                        + ::ferrum_hdl::bitpack::BitSize
                        + ::ferrum_hdl::bitpack::BitPack<
                            Packed = ::ferrum_hdl::bitvec::BitVec<{ < Self as ::ferrum_hdl::bitpack::BitSize >::BITS }>
                        >
                })));
                let where_clauses = utils::into_where_clause(predicates);

                quote! {
                    impl #impl_generics ::ferrum_hdl::trace::Traceable for #ident #ty_generics
                    #where_clauses
                    {
                        fn add_vars(vars: &mut ::ferrum_hdl::trace::TraceVars) {
                            <::ferrum_hdl::bitvec::BitVec<{ < Self as ::ferrum_hdl::bitpack::BitSize >::BITS }> as ::ferrum_hdl::trace::Traceable>::add_vars(vars);
                        }

                        fn trace(&self, id: &mut ::ferrum_hdl::trace::IdCode, tracer: &mut ::ferrum_hdl::trace::Tracer) -> ::std::io::Result<()> {
                            let bv: ::ferrum_hdl::bitvec::BitVec< { < Self as ::ferrum_hdl::bitpack::BitSize >::BITS } > = self.clone().pack();
                            bv.trace(id, tracer)
                        }
                    }
                }
            }
            AdtData::Struct(fields) => {
                let where_clauses = utils::into_where_clause(predicates);

                let vars = fields.iter().map(|field| {
                    let ident = field.ident.as_ref().unwrap().to_string();
                    let ty = &field.ty;

                    quote! {
                        vars.push_sym(#ident);
                        <#ty as ::ferrum_hdl::trace::Traceable>::add_vars(vars);
                        vars.pop();
                    }
                });

                let traces = fields.iter().map(|field| {
                    let field = field.ident.as_ref().unwrap();

                    quote! {
                        self.#field.trace(id, tracer)?;
                    }
                });

                quote! {
                    impl #impl_generics ::ferrum_hdl::trace::Traceable for #ident #ty_generics
                    #where_clauses
                    {
                        fn add_vars(vars: &mut ::ferrum_hdl::trace::TraceVars) {
                            #( #vars )*
                        }

                        fn trace(&self, id: &mut ::ferrum_hdl::trace::IdCode, tracer: &mut ::ferrum_hdl::trace::Tracer) -> ::std::io::Result<()> {
                            #( #traces )*

                            Ok(())
                        }
                    }
                }
            }
        }
    }
}