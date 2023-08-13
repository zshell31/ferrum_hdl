use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

#[proc_macro_attribute]
pub fn blackbox(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attr2: TokenStream2 = attr.into();
    let input2: TokenStream2 = input.into();

    quote! {
        #[ferrum_tool::blackbox(#attr2)]
        #input2
    }
    .into()
}
