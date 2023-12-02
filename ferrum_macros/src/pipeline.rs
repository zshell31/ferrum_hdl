use proc_macro2::{
    Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree,
};
use quote::TokenStreamExt;
use syn::parse::{Parse, ParseStream};

syn::custom_punctuation!(SignalMap, |>);
syn::custom_punctuation!(SignalAndThen, |>>);
syn::custom_punctuation!(SignalAnd, &&>);
syn::custom_punctuation!(SignalOr, ||>);

pub struct Pipeline {
    tokens: TokenStream,
}

impl Pipeline {
    pub fn into_token_stream(self) -> TokenStream {
        self.tokens
    }
}

enum Op {
    SignalMap,
    SignalAndThen,
    Nop,
}

fn extract(input: &ParseStream) -> syn::Result<(Op, TokenStream)> {
    let mut tokens = TokenStream::new();
    loop {
        if input.is_empty() {
            return Ok((Op::Nop, tokens));
        }
        if input.peek(SignalAndThen) {
            return Ok((Op::SignalAndThen, tokens));
        }
        if input.peek(SignalMap) {
            return Ok((Op::SignalMap, tokens));
        }

        tokens.append(input.parse::<TokenTree>()?);
    }
}

fn extract_non_empty(input: &ParseStream) -> syn::Result<(Op, TokenStream)> {
    match extract(input)? {
        (op, tokens) if !tokens.is_empty() => Ok((op, tokens)),
        _ => Err(input.error("expected expr")),
    }
}

impl Parse for Pipeline {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut tokens = TokenStream::new();
        loop {
            let next = extract(&input)?;
            match next {
                (Op::SignalMap, lhs) => {
                    let _ = input.parse::<SignalMap>()?;

                    tokens.extend(lhs);
                    tokens.extend([
                        TokenTree::Punct(Punct::new('.', Spacing::Alone)),
                        TokenTree::Ident(Ident::new("map", Span::call_site())),
                    ]);

                    let (_, rhs) = extract_non_empty(&input)?;

                    tokens.extend([TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        rhs,
                    ))]);
                }
                (Op::SignalAndThen, lhs) => {
                    let _ = input.parse::<SignalAndThen>()?;

                    tokens.extend(lhs);
                    tokens.extend([
                        TokenTree::Punct(Punct::new('.', Spacing::Alone)),
                        TokenTree::Ident(Ident::new("and_then", Span::call_site())),
                    ]);

                    let (_, rhs) = extract_non_empty(&input)?;

                    tokens.extend([TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        rhs,
                    ))]);
                }
                (Op::Nop, rest) => {
                    tokens.extend(rest);
                    break;
                }
            }
        }

        Ok(Pipeline { tokens })
    }
}
