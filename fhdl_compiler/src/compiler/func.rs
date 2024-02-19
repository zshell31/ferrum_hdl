use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{
    net_list::{ModuleId, NodeId},
    node::{Input, ModInst},
};
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
    error::{Error, SpanError, SpanErrorKind},
};

const STD_FUNCTIONS: &[&[&str]] = &[
    &["default", "Default", "default"],
    &["ops", "function", "Fn", "call"],
    &["ops", "function", "FnOnce", "call_once"],
    // Option
    &["option", "impl", "and"],
    &["option", "impl", "and_then"],
    &["option", "impl", "as_ref"],
    &["option", "impl", "filter"],
    &["option", "impl", "flatten"],
    &["option", "impl", "is_none"],
    &["option", "impl", "is_some"],
    &["option", "impl", "map"],
    &["option", "impl", "map_or"],
    &["option", "impl", "map_or_else"],
    &["option", "impl", "or"],
    &["option", "impl", "or_else"],
    &["option", "impl", "transpose"],
    &["option", "impl", "unzip"],
    &["option", "impl", "xor"],
    &["option", "impl", "zip"],
    &["option", "impl", "zip_with"],
    &["option", "impl", "unwrap_or"],
    &["option", "impl", "unwrap_or_default"],
    &["option", "impl", "unwrap_or_else"],
    // Result
    &["result", "impl", "and"],
    &["result", "impl", "and_then"],
    &["result", "impl", "as_ref"],
    &["result", "impl", "err"],
    &["result", "impl", "flatten"],
    &["result", "impl", "into_err"],
    &["result", "impl", "into_ok"],
    &["result", "impl", "is_err"],
    &["result", "impl", "is_err_and"],
    &["result", "impl", "is_ok"],
    &["result", "impl", "is_ok_and"],
    &["result", "impl", "map"],
    &["result", "impl", "map_err"],
    &["result", "impl", "map_or"],
    &["result", "impl", "map_or_else"],
    &["result", "impl", "ok"],
    &["result", "impl", "or"],
    &["result", "impl", "or_else"],
    &["result", "impl", "transpose"],
    &["result", "impl", "unwrap_or"],
    &["result", "impl", "unwrap_or_default"],
    &["result", "impl", "unwrap_or_else"],
];

fn def_path_eq(def_path: &DefPath, items: &[&'static str]) -> bool {
    let mut def_path = def_path.data.iter();
    let mut items = items.iter();

    loop {
        let def_path = def_path.next();
        let item = items.next();

        let is_matched = match (def_path, item) {
            (Some(def_path), Some(item)) => match &def_path.data {
                DefPathData::TypeNs(sym) | DefPathData::ValueNs(sym) => {
                    sym.as_str() == *item
                }
                DefPathData::Impl => *item == "impl",
                _ => false,
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

            // TODO: use aho-corasick for searching
            for std_func in STD_FUNCTIONS {
                if def_path_eq(def_path, std_func) {
                    return true;
                }
            }
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
