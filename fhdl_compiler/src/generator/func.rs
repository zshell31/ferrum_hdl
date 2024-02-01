use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{
    net_list::{ModuleId, NodeId},
    node::{Input, ModInst},
};
use rustc_hir::def_id::DefId;
use rustc_middle::ty::{FnSig, GenericArgsRef};
use rustc_span::{Span, Symbol as RustSymbol};

use super::{
    item::{Group, Item, ItemKind},
    item_ty::{ItemTy, ItemTyKind},
    locals::Local,
    Generator,
};
use crate::{
    blackbox::Blackbox,
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
};

impl<'tcx> Generator<'tcx> {
    pub fn fn_sig(&self, def_id: DefId, generics: GenericArgsRef<'tcx>) -> FnSig<'tcx> {
        let fn_sig = self.tcx.fn_sig(def_id);
        fn_sig.instantiate(self.tcx, generics).skip_binder()
    }

    pub fn make_input(
        &mut self,
        local: impl Into<Local>,
        ty: ItemTy<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Item<'tcx> {
        let local = local.into();
        let mod_id = ctx.module_id;
        let item = match &ty.kind() {
            ItemTyKind::Node(node_ty) => {
                let input = self
                    .netlist
                    .add_and_get_out(mod_id, Input::new(*node_ty, None));

                Item::new(ty, ItemKind::Node(input))
            }
            ItemTyKind::Module(_) => Item::new(ty, ItemKind::Module),
            ItemTyKind::Ref(ref_ty) => {
                let local = local.inc();
                let item = self.make_input(local, *ref_ty, ctx);
                ctx.locals.place(local, item);

                Item::new(ty, ItemKind::Ref(local))
            }
            ItemTyKind::Mut(ref_ty) => {
                let local = local.inc();
                let item = self.make_input(local, *ref_ty, ctx);
                ctx.locals.place(local, item);
                ctx.add_output(local);

                Item::new(ty, ItemKind::Mut(local))
            }
            ItemTyKind::Array(array_ty) => Item::new(
                ty,
                ItemKind::Group(Group::new(
                    array_ty.tys().map(|ty| self.make_input(local, ty, ctx)),
                )),
            ),
            ItemTyKind::Struct(struct_ty) => Item::new(
                ty,
                ItemKind::Group(Group::new(
                    struct_ty.tys().map(|ty| self.make_input(local, ty, ctx)),
                )),
            ),
            ItemTyKind::Enum(_) => {
                let node_ty = ty.to_bitvec();
                let input = self
                    .netlist
                    .add_and_get_out(mod_id, Input::new(node_ty, None));

                Item::new(ty, ItemKind::Node(input))
            }
        };

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
        ctx: &EvalContext<'tcx>,
    ) -> NodeId
    where
        'tcx: 'a,
    {
        let inputs = inputs
            .into_iter()
            .flat_map(|input| input.as_nodes(&ctx.locals));

        let outputs = self
            .netlist
            .mod_outputs(instant_mod_id)
            .map(|node_out_id| (self.netlist[node_out_id].ty, None));

        let mod_inst = ModInst::new(None, instant_mod_id, inputs, outputs);
        self.netlist.add(ctx.module_id, mod_inst)

        // self.combine_node_outputs(node_id, sig_ty)
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

    // pub fn is_blackbox(&self, def_id: DefId) -> bool {
    //     self.find_blackbox_(def_id).is_some()
    // }
}
