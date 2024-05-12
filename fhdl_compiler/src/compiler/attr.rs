use fhdl_common::{BlackboxKind, BlackboxTy, LangItem};
use rustc_ast::{
    token::{Lit, LitKind, Token, TokenKind},
    tokenstream::TokenTree,
    AttrArgs, AttrKind, DelimArgs,
};
use rustc_hir::def_id::DefId;
use rustc_middle::ty::TyCtxt;

use super::Compiler;

const FHDL_TOOL: &str = "fhdl_tool";
const SYNTH_ATTR: &str = "synth";
const BLACKBOX_ATTR: &str = "blackbox";
const BLACKBOX_TY_ATTR: &str = "blackbox_ty";
const LANG_ITEM_ATTR: &str = "lang_item";

#[derive(Debug, Default, Clone, Copy)]
pub struct SynthAttrs {
    pub inline: bool,
    pub top: bool,
}

pub fn find_fhdl_tool_attr<T>(
    tcx: TyCtxt<'_>,
    attr_kind: &str,
    def_id: DefId,
    parse_args: impl FnOnce(&AttrArgs) -> Option<T>,
) -> Option<T> {
    let attrs = tcx.get_attrs_unchecked(def_id);
    for attr in attrs {
        if let AttrKind::Normal(attr) = &attr.kind {
            let segments = &attr.item.path.segments;
            if segments.len() == 2
                && segments[0].ident.as_str() == FHDL_TOOL
                && segments[1].ident.as_str() == attr_kind
            {
                return parse_args(&attr.item.args);
            }
        }
    }

    None
}

pub fn find_lang_item(tcx: TyCtxt<'_>, def_id: DefId) -> Option<LangItem> {
    find_fhdl_tool_attr(tcx, LANG_ITEM_ATTR, def_id, |args| {
        let lang_item = extract_str_from_args(args)?;
        LangItem::try_from(lang_item).ok()
    })
}

fn extract_str_from_args(args: &AttrArgs) -> Option<&str> {
    if let AttrArgs::Delimited(DelimArgs { tokens, .. }) = args {
        if tokens.len() == 1 {
            if let Some(TokenTree::Token(
                Token {
                    kind:
                        TokenKind::Literal(Lit {
                            kind: LitKind::Str,
                            symbol,
                            ..
                        }),
                    ..
                },
                _,
            )) = tokens.trees().next()
            {
                return Some(symbol.as_str());
            }
        }
    }

    None
}

impl<'tcx> Compiler<'tcx> {
    fn find_fhdl_tool_attr<T>(
        &self,
        attr_kind: &str,
        def_id: DefId,
        parse_args: impl FnOnce(&AttrArgs) -> Option<T>,
    ) -> Option<T> {
        find_fhdl_tool_attr(self.tcx, attr_kind, def_id, parse_args)
    }

    pub fn find_blackbox_ty(&self, def_id: DefId) -> Option<BlackboxTy> {
        let blackbox_ty = self.find_fhdl_tool_attr(BLACKBOX_TY_ATTR, def_id, |args| {
            let blackbox_ty = extract_str_from_args(args)?;
            BlackboxTy::try_from(blackbox_ty).ok()
        })?;

        Some(blackbox_ty)
    }

    pub fn find_blackbox_kind(&self, def_id: DefId) -> Option<BlackboxKind> {
        if self.crates.is_ferrum_hdl(def_id) {
            self.find_fhdl_tool_attr(BLACKBOX_ATTR, def_id, |args| {
                let blackbox = extract_str_from_args(args)?;
                BlackboxKind::try_from(blackbox).ok()
            })
        } else {
            None
        }
    }

    pub fn find_synth(&self, def_id: DefId) -> Option<SynthAttrs> {
        self.find_fhdl_tool_attr(SYNTH_ATTR, def_id, |args| {
            let mut attrs = SynthAttrs::default();

            if let AttrArgs::Delimited(DelimArgs { tokens, .. }) = args {
                for token in tokens.trees() {
                    if let TokenTree::Token(
                        Token {
                            kind: TokenKind::Ident(symbol, ..),
                            ..
                        },
                        _,
                    ) = token
                    {
                        if symbol.as_str() == "inline" {
                            attrs.inline = true;
                        }
                        if symbol.as_str() == "top" {
                            attrs.top = true;
                        }
                    }
                }
            }

            Some(attrs)
        })
    }

    pub fn is_synth(&self, def_id: DefId) -> bool {
        self.find_synth(def_id).is_some()
    }

    pub fn is_top(&self, def_id: DefId) -> bool {
        self.find_synth(def_id)
            .map(|synth| synth.top)
            .unwrap_or_default()
    }

    pub fn is_blackbox_ty(&self, def_id: DefId) -> bool {
        self.find_blackbox_ty(def_id).is_some()
    }
}
