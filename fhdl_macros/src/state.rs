use darling::{ast::Data, FromDeriveInput};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use syn::{Field, GenericParam, Generics, Ident, Lifetime, LifetimeParam, Visibility};

use crate::utils::ferrum_hdl_crate;

#[derive(Debug, FromDeriveInput)]
#[darling(supports(struct_named))]
pub struct State {
    vis: Visibility,
    generics: Generics,
    ident: Ident,
    data: Data<(), Field>,
}

impl ToTokens for State {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let vis = &self.vis;
        let (impl_generics, ty_generics, where_bounds) = self.generics.split_for_impl();
        let ident = &self.ident;

        let fields = self.data.as_ref().take_struct().unwrap();

        let krate = ferrum_hdl_crate();

        let mk_state_fields = fields.iter().map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ty = &field.ty;

            quote! {
                #ident: <#ty as #krate::new_hdl::State>::state()
            }
        });

        let mut_ident = format_ident!("{ident}Mut");

        let mut_lf = Lifetime::new("'__state_mut", Span::call_site());
        let mut mut_generics = self.generics.clone();
        if !fields.is_empty() {
            mut_generics.params.insert(
                0,
                GenericParam::Lifetime(LifetimeParam::new(mut_lf.clone())),
            );
        }
        let (mut_impl_generics, mut_ty_generics, mut_where_bounds) =
            mut_generics.split_for_impl();
        let mut_lf = &mut_lf;

        let mut_fields = fields.iter().map(|field| {
            let vis = &field.vis;
            let ident = field.ident.as_ref().unwrap();
            let ty = &field.ty;

            quote! {
                #vis #ident: <#ty as #krate::new_hdl::State>::Mut<#mut_lf>
            }
        });

        let mk_mut_fields = fields.iter().map(|field| {
            let ident = field.ident.as_ref().unwrap();
            let ty = &field.ty;

            quote! {
                #ident: <#ty as #krate::new_hdl::State>::as_mut(&mut self.#ident)
            }
        });

        tokens.append_all(quote! {
            #vis struct #mut_ident #mut_generics #mut_where_bounds {
                #(#mut_fields),*
            }

            impl #mut_impl_generics #krate::new_hdl::StateMut for #mut_ident #mut_ty_generics #mut_where_bounds {}

            impl #impl_generics #krate::new_hdl::State for #ident #ty_generics #where_bounds {
                type Mut<#mut_lf> = #mut_ident #mut_ty_generics;

                fn state() -> Self {
                    Self {
                        #(#mk_state_fields),*
                    }
                }

                fn as_mut<#mut_lf>(&#mut_lf mut self) -> Self::Mut<#mut_lf> {
                    #mut_ident {
                        #(#mk_mut_fields),*
                    }
                }
            }
        });
    }
}
