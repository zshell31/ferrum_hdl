use fhdl_common::LangItem;
use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};

pub struct LangItemAttr(LangItem);

impl Parse for LangItemAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attr = input.parse::<Ident>()?;

        let value = attr.to_string();
        let kind = LangItem::try_from(value.as_str()).map_err(|_| {
            syn::Error::new(attr.span(), format!("Invalid lang_item '{}'", value))
        })?;

        Ok(Self(kind))
    }
}

impl ToTokens for LangItemAttr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let kind = self.0.to_string();
        tokens.extend(quote!(#kind));
    }
}
