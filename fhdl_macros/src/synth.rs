use std::collections::{HashMap, HashSet};

use darling::{ast::NestedMeta, util::Flag, FromMeta};
use fhdl_common::{
    Constraint, NonEmptyAsciiStr, NonEmptyStr, Pin, SynthAttrs as CommonSynthAttrs,
    Vendor,
};
use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use smallvec::{smallvec, SmallVec};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::{Iter, Punctuated},
    spanned::Spanned,
    token::Comma,
    Block, Expr, ExprArray, ExprBlock, ExprLit, ExprPath, ExprTuple, FieldPat, FnArg,
    ItemFn, Lit, Pat, PatIdent, PatReference, PatStruct, PatTuple, PatTupleStruct,
    PatType, Receiver, ReturnType, Stmt, Token, Type, TypeTuple,
};

use crate::utils::{MetaExt, SpannedValue};

#[derive(Debug)]
enum PinDescr {
    Short(NonEmptyAsciiStr),
    Detailed(Pin),
}

impl From<PinDescr> for Pin {
    fn from(descr: PinDescr) -> Self {
        match descr {
            PinDescr::Short(name) => Pin { name },
            PinDescr::Detailed(pin) => pin,
        }
    }
}

impl FromMeta for PinDescr {
    fn from_string(value: &str) -> darling::Result<Self> {
        Ok(Self::Short(NonEmptyAsciiStr::from_string(value)?))
    }

    fn from_expr(expr: &Expr) -> darling::Result<Self> {
        (match expr {
            Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) => Self::from_string(&s.value()),
            Expr::Tuple(tuple) => Pin::from_tuple(tuple).map(PinDescr::Detailed),
            Expr::Paren(paren) => Pin::from_paren(paren).map(PinDescr::Detailed),
            _ => Err(darling::Error::unexpected_expr_type(expr)),
        })
        .map_err(|e| e.with_span(&expr.span()))
    }
}

#[derive(Debug)]
struct PinDescrs(SmallVec<[PinDescr; 1]>);

impl PinDescrs {
    fn from_pin(pin: PinDescr) -> Self {
        Self(smallvec![pin])
    }

    fn from_pins(pins: SmallVec<[PinDescr; 1]>) -> darling::Result<Self> {
        Ok(Self(pins))
    }
}

impl From<PinDescrs> for SmallVec<[Pin; 1]> {
    fn from(pins: PinDescrs) -> Self {
        pins.0.into_iter().map(Into::into).collect()
    }
}

impl FromMeta for PinDescrs {
    fn from_value(lit: &Lit) -> darling::Result<Self> {
        PinDescr::from_value(lit).map(Self::from_pin)
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        PinDescr::from_string(value).map(Self::from_pin)
    }

    fn from_expr(expr: &Expr) -> darling::Result<Self> {
        match expr {
            Expr::Array(ExprArray { elems, .. }) => elems
                .iter()
                .map(PinDescr::from_expr)
                .collect::<Result<SmallVec<_>, _>>()
                .and_then(Self::from_pins),
            _ => PinDescr::from_expr(expr).map(Self::from_pin),
        }
    }
}

#[derive(Debug, FromMeta)]
struct Constraint_ {
    vendor: Vendor,
    name: SpannedValue<NonEmptyStr>,
    #[darling(default)]
    pins: SpannedValue<HashMap<Ident, PinDescrs>>,
}

impl Constraint_ {
    fn try_from_tuple(tuple: &ExprTuple) -> darling::Result<Self> {
        let this = Self::from_tuple(tuple)?;

        if !this.pins.is_empty() {
            Ok(this)
        } else {
            Err(darling::Error::custom("pins are not specified")
                .with_span(&this.pins.span()))
        }
    }
}

impl From<Constraint_> for Constraint {
    fn from(constr: Constraint_) -> Self {
        Constraint {
            vendor: constr.vendor,
            name: constr.name.into_inner(),
            pins: constr
                .pins
                .into_inner()
                .into_iter()
                .map(|(ident, pins)| (ident.to_string(), pins.into()))
                .collect(),
        }
    }
}

#[derive(Debug, Default)]
struct Constraints_(SmallVec<[Constraint_; 1]>);

impl Constraints_ {
    fn from_constraint(constr: Constraint_) -> Self {
        Self(smallvec![constr])
    }
}

impl FromMeta for Constraints_ {
    fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
        Constraint_::from_list(items).map(Self::from_constraint)
    }

    fn from_expr(expr: &Expr) -> darling::Result<Self> {
        match expr {
            Expr::Tuple(tuple) => {
                Constraint_::try_from_tuple(tuple).map(Self::from_constraint)
            }
            Expr::Array(ExprArray { elems, .. }) => {
                let constr = elems
                    .iter()
                    .map(|elem| {
                        if let Expr::Tuple(tuple) = elem {
                            Constraint_::try_from_tuple(tuple)
                        } else {
                            Err(darling::Error::unexpected_expr_type(elem)
                                .with_span(elem))
                        }
                    })
                    .collect::<Result<_, _>>()?;

                Ok(Self(constr))
            }
            _ => Ok(Self::default()),
        }
    }
}

#[derive(Debug, FromMeta)]
struct SynthAttrs_ {
    inline: Flag,
    #[darling(default)]
    constr: Constraints_,
}

impl Parse for SynthAttrs_ {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs: Vec<NestedMeta> =
            Punctuated::<NestedMeta, Token![,]>::parse_terminated(input)?
                .into_iter()
                .collect();
        let attrs = Self::from_list(&attrs)?;

        Ok(attrs)
    }
}

enum PatIter<'p> {
    Once(Option<&'p Pat>),
    Elems(Iter<'p, Pat>),
    Fields(Iter<'p, FieldPat>),
}

impl<'p> PatIter<'p> {
    fn once(pat: &'p Pat) -> Self {
        Self::Once(Some(pat))
    }

    fn elems(pat: &'p Punctuated<Pat, Comma>) -> Self {
        Self::Elems(pat.iter())
    }

    fn fields(fields: &'p Punctuated<FieldPat, Comma>) -> Self {
        Self::Fields(fields.iter())
    }
}

impl<'p> Iterator for PatIter<'p> {
    type Item = &'p Pat;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Once(once) => once.take(),
            Self::Elems(pat) => pat.next(),
            Self::Fields(fields) => fields.next().map(|field| field.pat.as_ref()),
        }
    }
}

fn extract_ident_from_pat(pat: PatIter<'_>, idents: &mut HashSet<Ident>) {
    for pat in pat {
        match pat {
            Pat::Type(PatType { pat, .. }) => {
                extract_ident_from_pat(PatIter::once(pat), idents);
            }
            Pat::Ident(PatIdent { ident, .. }) => {
                idents.insert(ident.clone());
            }
            Pat::Reference(PatReference { pat, .. }) => {
                extract_ident_from_pat(PatIter::once(pat), idents)
            }
            Pat::Struct(PatStruct { fields, .. }) => {
                extract_ident_from_pat(PatIter::fields(fields), idents)
            }
            Pat::Tuple(PatTuple { elems, .. }) => {
                extract_ident_from_pat(PatIter::elems(elems), idents)
            }
            Pat::TupleStruct(PatTupleStruct { elems, .. }) => {
                extract_ident_from_pat(PatIter::elems(elems), idents)
            }
            _ => {}
        }
    }
}

fn extract_ident_from_block(block: &Block, idents: &mut HashSet<Ident>) {
    if let Some(Stmt::Expr(expr, _)) = block.stmts.last() {
        extract_ident_from_expr(expr, idents);
    }
}

fn extract_ident_from_expr(expr: &Expr, idents: &mut HashSet<Ident>) {
    match expr {
        Expr::Path(ExprPath { path, .. }) => {
            if let Some(ident) = path.get_ident() {
                idents.insert(ident.clone());
            }
        }
        Expr::Tuple(ExprTuple { elems, .. }) => {
            for elem in elems {
                extract_ident_from_expr(elem, idents);
            }
        }
        Expr::Block(ExprBlock { block, .. }) => {
            extract_ident_from_block(block, idents);
        }
        _ => {}
    }
}

fn has_output(output: &ReturnType) -> bool {
    match output {
        ReturnType::Default => false,
        ReturnType::Type(_, ty) => {
            !matches!(ty.as_ref(), Type::Tuple(TypeTuple { elems, .. }) if elems.is_empty())
        }
    }
}

pub struct SynthAttrs(CommonSynthAttrs);

impl SynthAttrs {
    pub fn parse(attrs: TokenStream, input: &TokenStream) -> syn::Result<Self> {
        let attrs = syn::parse::<SynthAttrs_>(attrs)?;

        let mut idents = None;
        let mut names = HashSet::<String>::default();
        let mut constrs = SmallVec::with_capacity(attrs.constr.0.len());

        let mut errors = darling::Error::accumulator();

        for constr in attrs.constr.0 {
            let name = constr.name.to_lowercase();
            if names.contains(&name) {
                errors.push(
                    darling::Error::custom(format_args!(
                        "duplicate constraint name '{}'",
                        constr.name
                    ))
                    .with_span(&constr.name.span()),
                );
            }
            names.insert(name);

            if !constr.pins.is_empty() {
                if idents.is_none() {
                    let item_fn = syn::parse::<ItemFn>(input.clone())?;

                    idents = Some(HashSet::with_capacity(item_fn.sig.inputs.len()));
                    let idents = idents.as_mut().unwrap();

                    for fn_arg in &item_fn.sig.inputs {
                        match fn_arg {
                            FnArg::Receiver(Receiver { self_token, .. }) => {
                                idents.insert(Ident::new("self", self_token.span));
                            }
                            FnArg::Typed(PatType { pat, .. }) => {
                                extract_ident_from_pat(PatIter::once(pat), idents);
                            }
                        }
                    }

                    if has_output(&item_fn.sig.output) {
                        extract_ident_from_block(&item_fn.block, idents);
                    }
                }

                let idents = idents.as_ref().unwrap();

                for ident in idents {
                    if !constr.pins.contains_key(ident) {
                        errors.push(
                            darling::Error::custom(format_args!(
                                "pins are not specified for ident '{}' for constraint '{}'",
                                ident,
                                constr.name,
                            ))
                            .with_span(&ident.span()),
                        );
                    }
                }

                for ident in constr.pins.keys() {
                    if !idents.contains(ident) {
                        errors.push(
                            darling::Error::custom(format_args!(
                                "pins are specified for ident '{}' for constraint '{}' 
that is not input or output",
                                ident, constr.name
                            ))
                            .with_span(&ident.span()),
                        );
                    }
                }

                constrs.push(constr.into());
            }
        }

        errors.finish()?;

        let inline = attrs.inline.is_present();

        Ok(Self(CommonSynthAttrs {
            inline,
            constr: constrs,
        }))
    }
}

impl ToTokens for SynthAttrs {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let synth = &self.0;
        let attr = if synth.only_inline() {
            if synth.inline {
                quote!(inline)
            } else {
                quote!()
            }
        } else {
            let attr = serde_json::to_string(synth).unwrap();
            quote!(#attr)
        };

        tokens.extend(quote! {
            #[fhdl_tool::synth(#attr)]
        });
    }
}
