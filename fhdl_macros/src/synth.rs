use darling::{ast::NestedMeta, util::Flag, FromMeta};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Token,
};

#[derive(Debug, FromMeta)]
pub struct SynthAttrs {
    top: Flag,
    inline: Flag,
}

impl Parse for SynthAttrs {
    fn parse(attrs: ParseStream) -> syn::Result<Self> {
        let attrs: Vec<NestedMeta> =
            Punctuated::<NestedMeta, Token![,]>::parse_terminated(attrs)?
                .into_iter()
                .collect();

        Ok(Self::from_list(&attrs)?)
    }
}

impl ToTokens for SynthAttrs {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut attrs = Vec::with_capacity(1);
        if self.inline.is_present() {
            attrs.push(quote! { inline });
        }
        if self.top.is_present() {
            attrs.push(quote! { top });
        }

        tokens.extend(quote! {
            #[fhdl_tool::synth(#(#attrs),*)]
        });
    }
}
