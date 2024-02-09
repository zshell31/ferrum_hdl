use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{
    net_list::{ModuleId, NodeId},
    node::{Input, ModInst},
};
use rustc_hir::def_id::DefId;
use rustc_middle::{
    mir::Local,
    ty::{FnSig, GenericArgsRef},
};
use rustc_span::{Span, Symbol as RustSymbol};

use super::{item::Item, item_ty::ItemTy, Compiler, Context};
use crate::{
    blackbox::Blackbox,
    error::{Error, SpanError, SpanErrorKind},
};

impl<'tcx> Compiler<'tcx> {
    pub fn fn_sig(&self, def_id: DefId, generics: GenericArgsRef<'tcx>) -> FnSig<'tcx> {
        let fn_sig = self.tcx.fn_sig(def_id);
        fn_sig.instantiate(self.tcx, generics).skip_binder()
    }

    pub fn make_input(
        &mut self,
        local: Local,
        ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
    ) -> Item<'tcx> {
        let item = self.mk_item_from_ty(ty, ctx, &|compiler, node_ty, ctx| {
            compiler
                .netlist
                .add_and_get_out(ctx.module_id, Input::new(node_ty, None))
        });

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

    pub fn fn_output<T: Into<DefId>>(
        &mut self,
        fn_did: T,
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<ItemTy<'tcx>, Error> {
        let fn_sig = self.fn_sig(fn_did.into(), generics);
        self.resolve_ty(fn_sig.output(), generics, span)
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
        // TODO: check crate
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

    fn find_blackbox_(&self, def_id: DefId) -> Option<BlackboxKind> {
        if self.crates.is_core(def_id) {
            let def_path = self.tcx.def_path_str(def_id);

            if def_path == "std::clone::Clone::clone" {
                return Some(BlackboxKind::StdClone);
            }
        }

        self.find_blackbox_kind(def_id)
    }

    pub fn fn_name(&self, fn_did: DefId) -> String {
        self.tcx.def_path_str(fn_did)
    }
}
