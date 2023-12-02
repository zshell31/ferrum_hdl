use fhdl_blackbox::{BlackboxKind, BlackboxTy};
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};

pub struct BlackboxAttr(BlackboxKind);

impl Parse for BlackboxAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attr = input.parse::<Ident>()?;

        let value = attr.to_string();
        let kind = BlackboxKind::try_from(value.as_str()).map_err(|_| {
            syn::Error::new(attr.span(), format!("Invalid blackbox '{}'", value))
        })?;

        Ok(Self(kind))
    }
}

impl ToTokens for BlackboxAttr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let kind = self.0.to_string();
        tokens.extend(quote!(#kind));
    }
}

pub struct BlackboxTyAttr(BlackboxTy);

impl Parse for BlackboxTyAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attr = input.parse::<Ident>()?;

        let value = attr.to_string();
        let kind = BlackboxTy::try_from(value.as_str()).map_err(|_| {
            syn::Error::new(attr.span(), format!("Invalid blackbox ty '{}'", value))
        })?;

        Ok(Self(kind))
    }
}

impl ToTokens for BlackboxTyAttr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let kind = self.0.to_string();
        tokens.extend(quote!(#kind));
    }
}
