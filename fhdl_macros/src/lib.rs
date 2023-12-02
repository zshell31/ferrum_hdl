#![feature(rustc_private)]
mod bitpack;
mod blackbox;
mod impl_tuple_traits;
mod signal_value;
mod utils;

use bitpack::BitPackDerive;
use darling::FromDeriveInput;
use impl_tuple_traits::ImplTupleTraits;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use signal_value::SignalValueDerive;
use syn::{parse_macro_input, DeriveInput};

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
pub fn impl_tuple_traits(input: TokenStream) -> TokenStream {
    let impl_tuple = parse_macro_input!(input as ImplTupleTraits);

    impl_tuple.into_tokens().into()
}

#[proc_macro_derive(SignalValue, attributes(signal_value))]
pub fn derive_signal_value(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let parsed = match SignalValueDerive::from_derive_input(&input) {
        Ok(parsed) => parsed,
        Err(e) => return e.write_errors().into(),
    };

    parsed.into_tokens().into()
}

#[proc_macro_derive(BitPack, attributes(bitpack))]
pub fn derive_bitpack(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let parsed = match BitPackDerive::from_derive_input(&input) {
        Ok(parsed) => parsed,
        Err(e) => return e.write_errors().into(),
    };

    parsed.into_tokens().into()
}
