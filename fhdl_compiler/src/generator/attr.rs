use fhdl_blackbox::{BlackboxKind, BlackboxTy};
use rustc_ast::{
    token::{Lit, LitKind, Token, TokenKind},
    tokenstream::{LazyAttrTokenStream, TokenTree},
    AttrArgs, AttrKind, DelimArgs,
};
use rustc_hir::def_id::DefId;

use super::Generator;
use crate::error::Error;

const FHDL_TOOL: &str = "fhdl_tool";
const SYNTH_ATTR: &str = "synth";
const BLACKBOX_ATTR: &str = "blackbox";
const BLACKBOX_TY_ATTR: &str = "blackbox_ty";

impl<'tcx> Generator<'tcx> {
    pub fn find_fhdl_tool_attr<T>(
        &self,
        attr_kind: &str,
        def_id: DefId,
        parse_args: impl FnOnce(&AttrArgs) -> Option<T>,
    ) -> Option<T> {
        let attrs = self.tcx.get_attrs_unchecked(def_id);
        for attr in attrs {
            if let AttrKind::Normal(attr) = &attr.kind {
                let segments = &attr.item.path.segments;
                if segments.len() == 2
                    && segments[0].ident.as_str() == FHDL_TOOL
                    && segments[1].ident.as_str() == attr_kind
                {
                    if attr_kind == SYNTH_ATTR {
                        println!("{attr:#?}");
                    }
                    return parse_args(&attr.item.args);
                    // match &attr.item.args {
                    //     AttrArgs::Empty => {
                    //         return Some("");
                    //     }
                    //     AttrArgs::Delimited(DelimArgs { tokens, .. }) => {
                    //         if tokens.is_empty() {
                    //             return Some("");
                    //         }

                    //         if tokens.len() == 1 {
                    //             if let Some(TokenTree::Token(
                    //                 Token {
                    //                     kind:
                    //                         TokenKind::Literal(Lit {
                    //                             kind: LitKind::Str,
                    //                             symbol,
                    //                             ..
                    //                         }),
                    //                     ..
                    //                 },
                    //                 _,
                    //             )) = tokens.trees().next()
                    //             {
                    //                 return Some(symbol.as_str());
                    //             }
                    //         }
                    //     }
                    //     _ => {}
                    // }
                }
            }
        }

        None
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

    pub fn find_blackbox_ty(&self, def_id: DefId) -> Option<BlackboxTy> {
        let blackbox_ty = self.find_fhdl_tool_attr(BLACKBOX_TY_ATTR, def_id, |args| {
            let blackbox_ty = Self::extract_str_from_args(args)?;
            BlackboxTy::try_from(blackbox_ty).ok()
        })?;

        Some(blackbox_ty)
    }

    pub fn find_blackbox_kind(&self, def_id: DefId) -> Option<BlackboxKind> {
        if self.crates.is_ferrum_hdl(def_id) {
            self.find_fhdl_tool_attr(BLACKBOX_ATTR, def_id, |args| {
                let blackbox = Self::extract_str_from_args(args)?;
                BlackboxKind::try_from(blackbox).ok()
            })
        } else {
            None
        }
    }

    pub fn is_synth(&self, def_id: DefId) -> bool {
        match self.crates.is_ferrum_hdl_local() {
            false => {
                !def_id.is_local()
                    && self.find_fhdl_tool_attr(SYNTH_ATTR, def_id).is_some()
            }
            true => self.find_fhdl_tool_attr(SYNTH_ATTR, def_id).is_some(),
        }
    }

    pub fn is_blackbox_ty(&self, def_id: DefId) -> bool {
        self.find_fhdl_tool_attr(BLACKBOX_TY_ATTR, def_id).is_some()
    }
}
