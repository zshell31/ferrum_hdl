#![feature(rustc_private)]
mod bitpack;
mod bits;
mod blackbox;
mod impl_tuple_traits;
mod lang_item;
mod signal_value;
mod state;
mod synth;
mod traceable;
mod utils;

use bitpack::BitPack;
use bits::Bits;
use darling::FromDeriveInput;
use impl_tuple_traits::ImplTupleTraits;
use lang_item::LangItemAttr;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use signal_value::SignalValue;
use state::State;
use syn::{parse_macro_input, DeriveInput};
use synth::SynthAttrs;
use traceable::Traceable;

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

#[proc_macro_attribute]
pub fn lang_item(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input: TokenStream2 = input.into();
    let attr = parse_macro_input!(attr as LangItemAttr);

    quote! {
        #[fhdl_tool::lang_item(#attr)]
        #input
    }
    .into()
}

#[proc_macro_attribute]
pub fn synth(attrs: TokenStream, input: TokenStream) -> TokenStream {
    let attrs = match syn::parse::<SynthAttrs>(attrs) {
        Ok(attrs) => attrs,
        Err(e) => {
            return e.to_compile_error().into();
        }
    };
    let input: TokenStream2 = input.into();

    quote! {
        #attrs
        #input
    }
    .into()
}

#[proc_macro]
pub fn impl_tuple_traits(input: TokenStream) -> TokenStream {
    let impl_tuple = parse_macro_input!(input as ImplTupleTraits);

    impl_tuple.into_tokens().into()
}

#[proc_macro]
pub fn bits(input: TokenStream) -> TokenStream {
    let bits = parse_macro_input!(input as Bits);

    bits.into_tokens().into()
}

#[proc_macro_derive(SignalValue, attributes(signal_value))]
pub fn derive_signal_value(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let parsed = match SignalValue::from_derive_input(&input) {
        Ok(parsed) => parsed,
        Err(e) => return e.write_errors().into(),
    };

    parsed.into_tokens().into()
}

#[proc_macro_derive(BitPack, attributes(bitpack))]
pub fn derive_bitpack(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let parsed = match BitPack::from_derive_input(&input) {
        Ok(parsed) => parsed,
        Err(e) => return e.write_errors().into(),
    };

    match parsed.into_tokens() {
        Ok(tokens) => tokens.into(),
        Err(e) => e.write_errors().into(),
    }
}

#[proc_macro_derive(Traceable, attributes(traceable))]
pub fn derive_traceable(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let parsed = match Traceable::from_derive_input(&input) {
        Ok(parsed) => parsed,
        Err(e) => return e.write_errors().into(),
    };

    parsed.into_tokens().into()
}

#[proc_macro_derive(State)]
pub fn derive_state(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let parsed = match State::from_derive_input(&input) {
        Ok(parsed) => parsed,
        Err(e) => return e.write_errors().into(),
    };

    parsed.to_token_stream().into()
}
