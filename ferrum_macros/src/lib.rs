mod blackbox;

use blackbox::{BlackboxAttr, BlackboxTyAttr};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn blackbox(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input: TokenStream2 = input.into();
    let attr = parse_macro_input!(attr as BlackboxAttr);

    quote! {
        #[ferrum_tool::blackbox(#attr)]
        #input
    }
    .into()
}

#[proc_macro_attribute]
pub fn blackbox_ty(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input: TokenStream2 = input.into();
    let attr = parse_macro_input!(attr as BlackboxTyAttr);

    quote! {
        #[ferrum_tool::blackbox_ty(#attr)]
        #input
    }
    .into()
}
