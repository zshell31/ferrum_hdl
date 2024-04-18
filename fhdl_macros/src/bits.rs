use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    bracketed,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::{Bracket, Comma},
    Ident, LitInt, Token, Type,
};

mod kw {
    use syn::custom_keyword;

    custom_keyword!(L);
    custom_keyword!(H);
}

#[derive(Debug)]
enum IdentOrLit {
    Lit(usize),
    Ident(Ident),
}

impl Parse for IdentOrLit {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(LitInt) {
            let lit = input.parse::<LitInt>()?;
            Ok(Self::Lit(lit.base10_parse()?))
        } else if lookahead.peek(Ident) {
            let ident = input.parse::<Ident>()?;
            Ok(Self::Ident(ident))
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for IdentOrLit {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Lit(lit) => lit.to_tokens(tokens),
            Self::Ident(ident) => ident.to_tokens(tokens),
        }
    }
}

#[derive(Debug)]
enum BitPart {
    Bit(IdentOrLit),
    Range {
        start: IdentOrLit,
        end: IdentOrLit,
    },
    RangeOff {
        start: IdentOrLit,
        off: IdentOrLit,
        dir: bool,
    },
    Repeat {
        bit_parts: Punctuated<BitPart, Comma>,
        repeat: IdentOrLit,
    },
}

impl Parse for BitPart {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Bracket) {
            let (bit_parts, repeat) = parse_repeat(input)?;
            Ok(Self::Repeat { bit_parts, repeat })
        } else {
            let start = input.parse::<IdentOrLit>()?;
            if input.peek(Token![..]) {
                let _ = input.parse::<Token![..]>()?;
                let end = input.parse::<IdentOrLit>()?;
                Ok(Self::Range { start, end })
            } else if input.peek(Token![+]) {
                let _ = input.parse::<Token![+]>()?;
                let off = input.parse::<IdentOrLit>()?;
                Ok(Self::RangeOff {
                    start,
                    off,
                    dir: true,
                })
            } else if input.peek(Token![-]) {
                let _ = input.parse::<Token![-]>()?;
                let off = input.parse::<IdentOrLit>()?;
                Ok(Self::RangeOff {
                    start,
                    off,
                    dir: false,
                })
            } else {
                Ok(Self::Bit(start))
            }
        }
    }
}

impl BitPart {
    fn to_tokens(&self, var: &Ident) -> TokenStream {
        match self {
            Self::Bit(start) => {
                quote! {
                    #var.bit_const::<#start>()
                }
            }
            Self::Range { start, end } => {
                quote! {{
                    const W: usize = ::ferrum_hdl::const_functions::bit_width(#start, #end);
                    const START: usize = ::ferrum_hdl::const_functions::min(#start, #end);
                    #var.slice_const::<W, START>()
                }}
            }
            Self::RangeOff { start, off, dir } => {
                quote! {{
                    const W: usize = ::ferrum_hdl::const_functions::bit_width_off(#start, #off, #dir);
                    const START: usize = ::ferrum_hdl::const_functions::bit_start_off(#start, #off, #dir);
                    #var.slice_const::<W, START>()
                }}
            }
            Self::Repeat { bit_parts, repeat } => {
                let bit_parts = bit_parts.iter().map(|bit_part| bit_part.to_tokens(var));
                quote! {
                    Array::<#repeat, _>::repeat((#(#bit_parts),*))
                }
            }
        }
    }
}

#[derive(Debug)]
enum Part {
    Bits {
        var: Ident,
        bits: Punctuated<BitPart, Comma>,
    },
    Lit {
        width: IdentOrLit,
        lit: IdentOrLit,
        bits: Option<Punctuated<BitPart, Comma>>,
    },
    Repeat {
        parts: Punctuated<Part, Comma>,
        repeat: IdentOrLit,
    },
}

impl Parse for Part {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Bracket) {
            let (parts, repeat) = parse_repeat(input)?;
            Ok(Part::Repeat { parts, repeat })
        } else if input.peek(kw::L) || input.peek(kw::H) {
            let width = IdentOrLit::Lit(1);
            let lit = input.parse()?;
            let bits = if input.peek(Bracket) {
                let content;
                let _ = bracketed!(content in input);
                Some(Punctuated::parse_separated_nonempty(&content)?)
            } else {
                None
            };
            Ok(Self::Lit { width, lit, bits })
        } else if input.peek2(Token![#]) {
            let width = input.parse()?;
            let _ = input.parse::<Token![#]>()?;
            let lit = input.parse()?;
            let bits = if input.peek(Bracket) {
                let content;
                let _ = bracketed!(content in input);
                Some(Punctuated::parse_separated_nonempty(&content)?)
            } else {
                None
            };
            Ok(Self::Lit { width, lit, bits })
        } else {
            let var = input.parse()?;
            let content;
            let _ = bracketed!(content in input);
            let bits = Punctuated::parse_separated_nonempty(&content)?;
            Ok(Self::Bits { var, bits })
        }
    }
}

impl Part {
    fn to_tokens(&self) -> TokenStream {
        match self {
            Self::Bits { var, bits } => {
                let bits = bits.iter().map(|bits| bits.to_tokens(var));
                quote! {{
                    let #var = #var.pack();
                    (#(#bits),*)
                }}
            }
            Self::Lit { width, lit, bits } => {
                let lit = quote!( #lit.cast::<U<#width>>() );
                match bits {
                    Some(bits) => {
                        let var = Ident::new("var", Span::call_site());
                        let bits = bits.iter().map(|bits| bits.to_tokens(&var));
                        quote! {{
                            let #var = #lit;
                            (#(#bits),*)
                        }}
                    }
                    None => lit,
                }
            }
            Self::Repeat { parts, repeat } => {
                let parts = parts.iter().map(|part| part.to_tokens());
                quote! {
                    Array::<#repeat, _>::repeat((#(#parts),*))
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Bits {
    parts: Punctuated<Part, Comma>,
    cast: Option<Type>,
}

impl Parse for Bits {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let parts = Punctuated::parse_separated_nonempty(input)?;

        let cast = if input.peek(Token![as]) {
            let _ = input.parse::<Token![as]>()?;
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Self { parts, cast })
    }
}

impl Bits {
    pub fn into_tokens(self) -> TokenStream {
        let parts = self.parts.iter().map(|part| part.to_tokens());

        match self.cast {
            Some(cast) => {
                quote! {
                    (#(#parts),*).repack::<#cast>()
                }
            }
            None => {
                quote! {
                    (#(#parts),*).repack()
                }
            }
        }
    }
}

fn parse_repeat<T: Parse>(
    input: ParseStream,
) -> syn::Result<(Punctuated<T, Comma>, IdentOrLit)> {
    let content;
    let _ = bracketed!(content in input);
    let items = Punctuated::parse_separated_nonempty(&content)?;
    let _ = content.parse::<Token![;]>()?;
    let repeat = content.parse()?;

    Ok((items, repeat))
}
