mod blackbox;
mod pipeline;

use pipeline::Pipeline;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse_macro_input;

use self::blackbox::{BlackboxAttr, BlackboxTyAttr};

#[proc_macro_attribute]
pub fn blackbox(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input: TokenStream2 = input.into();
    let attr = parse_macro_input!(attr as BlackboxAttr);

    quote! {
        #[fhdl_tool::blackbox(#attr)]
        #input
    }
    .into()
}

#[proc_macro_attribute]
pub fn blackbox_ty(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input: TokenStream2 = input.into();
    let attr = parse_macro_input!(attr as BlackboxTyAttr);

    quote! {
        #[fhdl_tool::blackbox_ty(#attr)]
        #input
    }
    .into()
}

#[proc_macro]
pub fn p(input: TokenStream) -> TokenStream {
    let pipeline = parse_macro_input!(input as Pipeline);

    pipeline.into_token_stream().into()
}

// #[proc_macro_derive(SingleValue)]
// pub fn derive_single_value(input: TokenStream) -> TokenStream {
//     input
// }
