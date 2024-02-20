use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{
    net_list::{ModuleId, NodeId},
    node::{Input, ModInst},
};
use once_cell::sync::Lazy;
use rustc_hir::{
    def::DefKind,
    def_id::DefId,
    definitions::{DefPath, DefPathData},
};
use rustc_middle::{
    mir::Local,
    ty::{ClosureArgs, FnSig, GenericArgsRef, Ty},
};
use rustc_span::{Span, Symbol as RustSymbol};
use tracing::debug;

use super::{item::Item, item_ty::ItemTy, Compiler, Context};
use crate::{
    blackbox::Blackbox,
    compiler::trie::Trie,
    error::{Error, SpanError, SpanErrorKind},
};

const IMPL: &str = "impl";

static STD_FUNCTIONS: Lazy<Trie> = Lazy::new(|| {
    let std_func: &[&[&str]] = &[
        &["default", "Default", "default"],
        &["ops", "function", "Fn", "call"],
        &["ops", "function", "FnOnce", "call_once"],
        // Option
        &["option", IMPL, "and"],
        &["option", IMPL, "and_then"],
        &["option", IMPL, "as_ref"],
        &["option", IMPL, "filter"],
        &["option", IMPL, "flatten"],
        &["option", IMPL, "is_none"],
        &["option", IMPL, "is_some"],
        &["option", IMPL, "map"],
        &["option", IMPL, "map_or"],
        &["option", IMPL, "map_or_else"],
        &["option", IMPL, "or"],
        &["option", IMPL, "or_else"],
        &["option", IMPL, "transpose"],
        &["option", IMPL, "unzip"],
        &["option", IMPL, "xor"],
        &["option", IMPL, "zip"],
        &["option", IMPL, "zip_with"],
        &["option", IMPL, "unwrap_or"],
        &["option", IMPL, "unwrap_or_default"],
        &["option", IMPL, "unwrap_or_else"],
        // Result
        &["result", IMPL, "and"],
        &["result", IMPL, "and_then"],
        &["result", IMPL, "as_ref"],
        &["result", IMPL, "err"],
        &["result", IMPL, "flatten"],
        &["result", IMPL, "into_err"],
        &["result", IMPL, "into_ok"],
        &["result", IMPL, "is_err"],
        &["result", IMPL, "is_err_and"],
        &["result", IMPL, "is_ok"],
        &["result", IMPL, "is_ok_and"],
        &["result", IMPL, "map"],
        &["result", IMPL, "map_err"],
        &["result", IMPL, "map_or"],
        &["result", IMPL, "map_or_else"],
        &["result", IMPL, "ok"],
        &["result", IMPL, "or"],
        &["result", IMPL, "or_else"],
        &["result", IMPL, "transpose"],
        &["result", IMPL, "unwrap_or"],
        &["result", IMPL, "unwrap_or_default"],
        &["result", IMPL, "unwrap_or_else"],
    ];

    let mut trie = Trie::new();
    for path in std_func {
        trie.add(path);
    }

    trie
});

enum SymOrStr {
    Sym(RustSymbol),
    Str(&'static str),
}

impl AsRef<str> for SymOrStr {
    fn as_ref(&self) -> &str {
        match self {
            Self::Sym(sym) => sym.as_str(),
            Self::Str(s) => s,
        }
    }
}

fn def_path_data(def_path_data: &DefPathData) -> Option<SymOrStr> {
    match &def_path_data {
        DefPathData::TypeNs(sym) | DefPathData::ValueNs(sym) => Some(SymOrStr::Sym(*sym)),
        DefPathData::Impl => Some(SymOrStr::Str(IMPL)),
        _ => None,
    }
}

fn def_path_eq(def_path: &DefPath, items: &[&'static str]) -> bool {
    let mut def_path = def_path.data.iter();
    let mut items = items.iter();

    loop {
        let def_path = def_path.next();
        let item = items.next();

        let is_matched = match (def_path, item) {
            (Some(def_path), Some(item)) => match def_path_data(&def_path.data) {
                Some(data) => data.as_ref() == *item,
                None => false,
            },
            (Some(_), None) | (None, Some(_)) => false,
            (None, None) => {
                return true;
            }
        };

        if !is_matched {
            break;
        }
    }

    false
}

impl<'tcx> Compiler<'tcx> {
    pub fn fn_sig(&self, def_id: DefId, generics: GenericArgsRef<'tcx>) -> FnSig<'tcx> {
        if let DefKind::Closure = self.tcx.def_kind(def_id) {
            let closure_args = ClosureArgs { args: generics };

            closure_args.sig().skip_binder()
        } else {
            let fn_sig = self.tcx.fn_sig(def_id);
            fn_sig.instantiate(self.tcx, generics).skip_binder()
        }
    }

    pub fn fn_inputs(
        &self,
        def_id: DefId,
        generics: GenericArgsRef<'tcx>,
    ) -> &'tcx [Ty<'tcx>] {
        let fn_sig = self.fn_sig(def_id, generics);
        let fn_inputs = fn_sig.inputs();
        if let DefKind::Closure = self.tcx.def_kind(def_id) {
            // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/struct.ClosureArgs.html
            // ```
            // CS represents the closure signature, representing as a fn() type.
            // For example, fn(u32, u32) -> u32 would mean that the closure implements CK<(u32, u32), Output = u32>,
            // where CK is the trait specified above
            // ```
            fn_inputs[0].tuple_fields()
        } else {
            fn_inputs
        }
    }

    pub fn closure_inputs(&self, ty: &ItemTy<'tcx>) -> &'tcx [Ty<'tcx>] {
        let ty = ty.closure_ty();
        self.fn_inputs(ty.fn_did, ty.fn_generics)
    }

    pub fn fn_output(&self, def_id: DefId, generics: GenericArgsRef<'tcx>) -> Ty<'tcx> {
        let fn_sig = self.fn_sig(def_id, generics);
        fn_sig.output()
    }

    pub fn make_input(
        &mut self,
        local: Local,
        ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
    ) -> Item<'tcx> {
        let item = self
            .mk_item_from_ty(ty, ctx, &|compiler, node_ty, ctx| {
                Some(
                    compiler
                        .netlist
                        .add_and_get_out(ctx.module_id, Input::new(node_ty, None)),
                )
            })
            .unwrap();

        // self.assign_names_to_item(ident, &item, force)

        ctx.locals.place(local, item.clone());

        item
    }

    pub fn is_inlined(&self, did: DefId) -> bool {
        self.tcx
            .get_attrs(did, RustSymbol::intern("inline"))
            .next()
            .is_some()
            || self.find_synth(did).map(|synth| synth.inlined).is_some()
    }

    pub fn is_std_call(&self, fn_did: DefId) -> bool {
        if self.crates.is_std(fn_did) {
            debug!("is_std_call: fn_did = {fn_did:?}");
            let def_path = &self.tcx.def_path(fn_did);

            return STD_FUNCTIONS.find(
                def_path
                    .data
                    .iter()
                    .filter_map(|def_path| def_path_data(&def_path.data)),
            );
        }

        false
    }

    pub fn instantiate_module<'a>(
        &mut self,
        instant_mod_id: ModuleId,
        inputs: impl IntoIterator<Item = &'a Item<'tcx>> + 'a,
        ctx: &Context<'tcx>,
    ) -> NodeId
    where
        'tcx: 'a,
    {
        let inputs = inputs.into_iter().flat_map(|input| input.iter());

        let outputs = self
            .netlist
            .mod_outputs(instant_mod_id)
            .map(|node_out_id| (self.netlist[node_out_id].ty, None));

        let mod_inst = ModInst::new(None, instant_mod_id, inputs, outputs);

        self.netlist.add(ctx.module_id, mod_inst)
    }

    pub fn find_blackbox(
        &mut self,
        fn_did: DefId,
        span: Span,
    ) -> Result<Blackbox, Error> {
        #[allow(clippy::map_entry)]
        if !self.blackbox.contains_key(&fn_did) {
            let blackbox = self.find_blackbox_(fn_did);

            self.blackbox.insert(fn_did, blackbox);
        }

        self.blackbox
            .get(&fn_did)
            .unwrap()
            .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthCall, span))
            .map(|kind| Blackbox { kind, fn_did })
            .map_err(Into::into)
    }

    pub fn has_blackbox(&mut self, fn_did: DefId) -> bool {
        self.blackbox.contains_key(&fn_did) || self.find_blackbox_(fn_did).is_some()
    }

    fn find_blackbox_(&self, def_id: DefId) -> Option<BlackboxKind> {
        if self.crates.is_std(def_id) {
            let def_path = self.tcx.def_path(def_id);

            if def_path_eq(&def_path, &["clone", "Clone", "clone"]) {
                return Some(BlackboxKind::StdClone);
            }
        }

        self.find_blackbox_kind(def_id)
    }

    pub fn fn_name(&self, fn_did: DefId) -> String {
        self.tcx.def_path_str(fn_did)
    }
}
