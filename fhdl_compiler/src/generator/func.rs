use std::{borrow::Cow, collections::VecDeque};

use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{
    group::ItemId,
    net_list::ModuleId,
    node::{Input, ModInst, NodeKind},
    resolver::Resolve,
    sig_ty::{NodeTy, SignalTy, SignalTyKind},
    symbol::Symbol,
};
use rustc_ast::{Mutability, UintTy};
use rustc_hir::{
    def::Res,
    def_id::{DefId, LocalDefId},
    BodyId, Expr, FnDecl, FnSig as HirFnSig, ImplItem, ImplItemKind, Item, ItemKind,
    MutTy, Param, PrimTy as HirPrimTy, QPath, Ty as HirTy, TyKind as HirTyKind,
};
use rustc_middle::ty::{FnSig, GenericArgsRef, TyKind};
use rustc_span::{symbol::Ident, Span, Symbol as RustSymbol};
use smallvec::SmallVec;

use super::{
    expr::ExprOrItemId,
    metadata::{
        resolver::{ImportModule, MetadataResolver},
        Metadata, TemplateNodeKind,
    },
    Generator, MonoItem,
};
use crate::{
    blackbox::{cast::Conversion, Blackbox},
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    scopes::SymIdent,
    utils,
};

impl<'tcx> Generator<'tcx> {
    pub fn fn_sig(&self, def_id: DefId, ctx: &EvalContext<'tcx>) -> FnSig<'tcx> {
        let fn_sig = self.tcx.fn_sig(def_id);
        ctx.instantiate_early_binder(self.tcx, fn_sig).skip_binder()
    }

    pub fn eval_fn_item(
        &mut self,
        item: &Item<'tcx>,
        top_module: bool,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Result<ModuleId, Error> {
        if let ItemKind::Fn(HirFnSig { decl, .. }, _, body_id) = item.kind {
            return self.eval_fn(
                item.ident.as_str(),
                decl,
                body_id,
                top_module,
                generic_args,
            );
        }

        Err(SpanError::new(SpanErrorKind::NotSynthItem, item.span).into())
    }

    pub fn eval_impl_item(
        &mut self,
        impl_item: &ImplItem<'tcx>,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Result<ModuleId, Error> {
        let self_ty = self
            .tcx
            .hir()
            .get_parent(impl_item.hir_id())
            .expect_item()
            .expect_impl()
            .self_ty;
        let ident: Cow<str> = match self_ty.kind {
            HirTyKind::Path(QPath::Resolved(_, path), ..) => {
                let prefix = path
                    .segments
                    .iter()
                    .map(|segment| segment.ident.as_str())
                    .intersperse("$")
                    .collect::<String>();

                format!("{}${}", prefix, impl_item.ident.as_str()).into()
            }
            _ => impl_item.ident.as_str().into(),
        };
        if let ImplItemKind::Fn(HirFnSig { decl, .. }, body_id) = impl_item.kind {
            return self.eval_fn(ident.as_ref(), decl, body_id, false, generic_args);
        }

        Err(SpanError::new(SpanErrorKind::NotSynthItem, impl_item.span).into())
    }

    pub fn eval_fn(
        &mut self,
        name: &str,
        fn_decl: &FnDecl<'tcx>,
        body_id: BodyId,
        top_module: bool,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Result<ModuleId, Error> {
        let body = self.tcx.hir().body(body_id);
        let inputs = fn_decl.inputs.iter().zip(body.params.iter());

        let module_sym = Symbol::new(name);
        let module_id = self.netlist.add_module(module_sym, top_module);

        self.idents.for_module(module_id).push_scope();

        let mut ctx = EvalContext::new(self.is_primary, generic_args, module_id);

        self.eval_inputs(inputs, &mut ctx, false)?;
        let item_id = self.eval_expr(body.value, &mut ctx)?;
        self.eval_outputs(item_id);

        self.idents.for_module(module_id).pop_scope();

        Ok(module_id)
    }

    pub fn eval_inputs<'a>(
        &mut self,
        inputs: impl Iterator<Item = (&'a HirTy<'tcx>, &'a Param<'tcx>)>,
        ctx: &mut EvalContext<'tcx>,
        is_closure: bool,
    ) -> Result<(), Error>
    where
        'tcx: 'a,
    {
        for (input, param) in inputs {
            let ty = self.node_type(param.hir_id, ctx);
            if let TyKind::Adt(adt, ..) = ty.kind() {
                if self.ignore_ty(adt.did()) {
                    continue;
                }
            }

            let item_id = self.make_input(input, ctx, is_closure)?;
            self.pattern_match(param.pat, item_id, ctx.module_id)?;
        }

        Ok(())
    }

    fn make_input(
        &mut self,
        input: &HirTy<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        is_closure: bool,
    ) -> Result<ItemId, Error> {
        match input.kind {
            HirTyKind::Infer => {
                let sig_ty =
                    self.find_sig_ty(self.node_type(input.hir_id, ctx), ctx, input.span)?;

                Ok(self.make_input_with_sig_ty(sig_ty, ctx, is_closure))
            }
            HirTyKind::Path(QPath::Resolved(_, path)) => {
                let fn_id = input.hir_id.owner.def_id;

                let (is_self_param, sig_ty) = match path.res {
                    Res::Def(..) => (
                        false,
                        self.find_sig_ty_for_hir_ty(fn_id, input, ctx, input.span)?,
                    ),
                    Res::SelfTyAlias { .. } => (
                        true,
                        self.find_sig_ty_for_hir_ty(fn_id, input, ctx, input.span)?,
                    ),
                    Res::PrimTy(HirPrimTy::Bool) => {
                        (false, SignalTy::new(NodeTy::Bool.into()))
                    }
                    Res::PrimTy(HirPrimTy::Uint(UintTy::U8)) => {
                        (false, SignalTy::new(NodeTy::U8.into()))
                    }
                    Res::PrimTy(HirPrimTy::Uint(UintTy::U16)) => {
                        (false, SignalTy::new(NodeTy::U16.into()))
                    }
                    Res::PrimTy(HirPrimTy::Uint(UintTy::U32)) => {
                        (false, SignalTy::new(NodeTy::U32.into()))
                    }
                    Res::PrimTy(HirPrimTy::Uint(UintTy::U64)) => {
                        (false, SignalTy::new(NodeTy::U64.into()))
                    }
                    Res::PrimTy(HirPrimTy::Uint(UintTy::U128)) => {
                        (false, SignalTy::new(NodeTy::U128.into()))
                    }
                    Res::PrimTy(HirPrimTy::Uint(UintTy::Usize)) => {
                        (false, SignalTy::new(NodeTy::Usize.into()))
                    }
                    _ => panic!("Cannot define def_id for {:?}", path.res),
                };

                let input = self.make_input_with_sig_ty(sig_ty, ctx, is_closure);

                if is_self_param {
                    self.idents
                        .for_module(ctx.module_id)
                        .add_local_ident(Ident::from_str("self"), input);
                }

                Ok(input)
            }
            HirTyKind::Ref(
                _,
                MutTy {
                    ty,
                    mutbl: Mutability::Not,
                },
            ) => self.make_input(ty, ctx, is_closure),
            HirTyKind::Tup(ty) => {
                let tuple_ty = self
                    .find_sig_ty(self.node_type(input.hir_id, ctx), ctx, input.span)?
                    .struct_ty();

                self.make_struct_group(tuple_ty, ty.iter(), |generator, ty| {
                    generator.make_input(ty, ctx, is_closure)
                })
            }
            _ => {
                println!("input: {:#?}", input);
                Err(SpanError::new(SpanErrorKind::NotSynthInput, input.span).into())
            }
        }
    }

    fn make_input_with_sig_ty(
        &mut self,
        sig_ty: SignalTy,
        ctx: &mut EvalContext<'tcx>,
        is_closure: bool,
    ) -> ItemId {
        let module_id = ctx.module_id;

        match sig_ty.kind {
            SignalTyKind::Node(prim_ty) => (if is_closure {
                ctx.next_closure_input()
            } else {
                let input = Input::new(prim_ty, None);
                self.netlist.add_and_get_out(module_id, input)
            })
            .into(),
            SignalTyKind::Array(ty) => self
                .make_array_group(ty, ty.tys(), |generator, ty| {
                    Ok(generator.make_input_with_sig_ty(ty, ctx, is_closure))
                })
                .unwrap(),
            SignalTyKind::Struct(ty) => self
                .make_struct_group(
                    ty,
                    ty.tys().iter().map(|ty| ty.inner),
                    |generator, ty| {
                        Ok(generator.make_input_with_sig_ty(ty, ctx, is_closure))
                    },
                )
                .unwrap(),
            SignalTyKind::Enum(ty) => (if is_closure {
                ctx.next_closure_input()
            } else {
                let input = Input::new(ty.prim_ty(), None);
                self.netlist.add_and_get_out(module_id, input)
            })
            .into(),
        }
    }

    fn is_inlined<T: Into<DefId>>(&self, did: T) -> bool {
        self.tcx
            .get_attrs(did.into(), RustSymbol::intern("inline"))
            .next()
            .is_some()
    }

    fn eval_fn_inputs(
        &mut self,
        args: impl IntoIterator<Item = ExprOrItemId<'tcx>>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<SmallVec<[ItemId; 4]>, Error> {
        args.into_iter()
            .map(|arg| arg.evaluate(self, ctx))
            .collect()
    }

    fn eval_outputs(&mut self, item_id: ItemId) {
        for node_out_id in item_id.into_iter() {
            let out = &mut self.netlist[node_out_id];
            if out.sym.is_none() {
                out.sym = SymIdent::Out.into();
            }
            self.netlist.add_output(node_out_id);
        }
    }

    fn fn_output<T: Into<DefId>>(
        &mut self,
        fn_did: T,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<SignalTy, Error> {
        let fn_sig = self.fn_sig(fn_did.into(), ctx);
        self.find_sig_ty(fn_sig.output(), ctx, span)
    }

    pub fn eval_fn_call(
        &mut self,
        fn_did: LocalDefId,
        fn_generics: GenericArgsRef<'tcx>,
        args: impl IntoIterator<Item = ExprOrItemId<'tcx>>,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let item = self.tcx.hir().expect_item(fn_did);

        let module_id = if self.is_primary {
            let mono_item = MonoItem::new(fn_did, fn_generics);

            #[allow(clippy::map_entry)]
            if !self.evaluated_modules.contains_key(&mono_item) {
                let module_id = self.eval_fn_item(item, false, fn_generics)?;

                self.evaluated_modules.insert(mono_item, module_id);
            }

            *self.evaluated_modules.get(&mono_item).unwrap()
        } else {
            let module_id = self.eval_fn_item(item, false, fn_generics)?;

            self.metadata.add_module_id(fn_did, module_id);

            module_id
        };

        // use fn generics to get output_ty because output_ty is the part of the fn signature
        let output_ty =
            self.fn_output(fn_did, &ctx.with_generic_args(fn_generics), span)?;
        // but ctx.generic_args to evaluate arguments of the function because arguments are is the
        // part of the caller and may require the generics of the caller
        let args = self.eval_fn_inputs(args, ctx)?;
        let inlined = self.is_inlined(fn_did);

        Ok(self.instantiate_module(module_id, args, inlined, output_ty, ctx))
    }

    pub fn eval_impl_fn_call(
        &mut self,
        impl_id: LocalDefId,
        fn_generics: GenericArgsRef<'tcx>,
        self_arg: Option<ExprOrItemId<'tcx>>,
        args: impl IntoIterator<Item = ExprOrItemId<'tcx>>,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let impl_item = self.tcx.hir().expect_impl_item(impl_id);

        let module_id = if self.is_primary {
            let mono_item = MonoItem::new(impl_id, fn_generics);

            #[allow(clippy::map_entry)]
            if !self.evaluated_modules.contains_key(&mono_item) {
                let module_id = self.eval_impl_item(impl_item, fn_generics)?;

                self.evaluated_modules.insert(mono_item, module_id);
            }

            *self.evaluated_modules.get(&mono_item).unwrap()
        } else {
            let module_id = self.eval_impl_item(impl_item, fn_generics)?;

            self.metadata.add_module_id(impl_id, module_id);

            module_id
        };

        // use fn generics to get output_ty because output_ty is the part of the fn signature
        let output_ty =
            self.fn_output(impl_id, &ctx.with_generic_args(fn_generics), span)?;
        // but ctx.generic_args to evaluate arguments of the function because arguments are is the
        // part of the caller and may require the generics of the caller
        let inputs = self.eval_fn_inputs(self_arg.into_iter().chain(args), ctx)?;
        let inlined = self.is_inlined(impl_id);

        Ok(self.instantiate_module(module_id, inputs, inlined, output_ty, ctx))
    }

    fn instantiate_module(
        &mut self,
        instant_mod_id: ModuleId,
        inputs: SmallVec<[ItemId; 4]>,
        inlined: bool,
        sig_ty: SignalTy,
        ctx: &EvalContext<'tcx>,
    ) -> ItemId {
        let inputs = inputs.into_iter().flat_map(|input| input.into_iter());

        let outputs = self
            .netlist
            .mod_outputs(instant_mod_id)
            .map(|node_out_id| (self.netlist[node_out_id].ty, None));

        let mod_inst = ModInst::new(None, instant_mod_id, inlined, inputs, outputs);
        let node_id = self.netlist.add(ctx.module_id, mod_inst);

        self.combine_outputs(node_id, sig_ty)
    }

    pub fn eval_synth_fn_or_blackbox(
        &mut self,
        fn_did: DefId,
        expr: &'tcx Expr<'tcx>,
        fn_generics: GenericArgsRef<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        if self.is_synth(fn_did) {
            self.eval_synth_fn(fn_did, expr, fn_generics, ctx, span)
        } else {
            let blackbox = self.find_blackbox(fn_did, span)?;
            blackbox.eval_expr(self, expr, ctx)
        }
    }

    pub fn eval_synth_fn(
        &mut self,
        fn_did: DefId,
        expr: &'tcx Expr<'tcx>,
        fn_generics: GenericArgsRef<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let krate = fn_did.krate;
        let metadata = self.find_metadata_for_crate(krate)?;

        let module_id = metadata.find_module_id_by_def_id(fn_did).ok_or_else(|| {
            SpanError::new(
                SpanErrorKind::MissingModule(self.tcx.def_path_str(fn_did)),
                span,
            )
        })?;

        let module_id = if self.is_primary {
            let mono_item = MonoItem::new(fn_did, ctx.generic_args);

            #[allow(clippy::map_entry)]
            if !self.evaluated_modules.contains_key(&mono_item) {
                // resolve all types from the imported module using fn_generics
                let module_id = self.clone_module_from_metadata(
                    module_id,
                    &metadata,
                    fn_did,
                    &ctx.with_generic_args(fn_generics),
                    span,
                )?;

                self.evaluated_modules.insert(mono_item, module_id);
            }

            *self.evaluated_modules.get(&mono_item).unwrap()
        } else {
            panic!("Evaluation synthesizable function for non-primary crate");
        };

        // use fn generics to get output_ty because output_ty is the part of the fn signature
        let output_ty =
            self.fn_output(fn_did, &ctx.with_generic_args(fn_generics), span)?;
        // but ctx.generic_args to evaluate arguments of the function because arguments are is the
        // part of the caller and may require the generics of the caller
        let inputs = self.eval_fn_inputs(
            utils::expected_call(expr)?.args.into_iter().map(Into::into),
            ctx,
        )?;
        let is_inlined = self.is_inlined(fn_did);

        Ok(self.instantiate_module(module_id, inputs, is_inlined, output_ty, ctx))
    }

    fn clone_module_from_metadata(
        &mut self,
        module_id: ModuleId,
        metadata: &Metadata<'tcx>,
        fn_did: DefId,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<ModuleId, Error> {
        let netlist = metadata.netlist();
        let mut modules_to_eval = {
            let mut v = VecDeque::with_capacity(8);

            let source = &netlist[module_id];
            let target = self.netlist.add_module(source.name, false);

            v.push_back(ImportModule {
                source: module_id,
                target,
            });
            v
        };

        let mut res = None;

        while let Some(ImportModule { source, target }) = modules_to_eval.pop_front() {
            let mut cursor = netlist.mod_cursor(source);
            while let Some(node_id) = netlist.next(&mut cursor) {
                let mut resolver = MetadataResolver::new(
                    self,
                    metadata,
                    fn_did,
                    &mut modules_to_eval,
                    ctx,
                    span,
                );
                let node = netlist[node_id].resolve_kind(&mut resolver)?;

                match node {
                    NodeKind::TemplateNode(node) => {
                        let id = node.temp_node_id();
                        let gen_node = metadata.template_node(id).ok_or_else(|| {
                            SpanError::new(SpanErrorKind::MissingTemplateNode(id), span)
                        })?;

                        let item_id = match gen_node {
                            TemplateNodeKind::CastToUnsigned { from, to_ty } => {
                                let from = from.with_module_id(target).into();
                                let to_ty = to_ty.resolve(&mut resolver)?;
                                Conversion::to_unsigned(from, to_ty, self)
                            }
                        };

                        let node_out_id = item_id.node_out_id();
                        let new_node_id = node_out_id.node_id();

                        assert_eq!(
                            self.netlist[new_node_id].inputs_len(),
                            netlist[node_id].inputs_len()
                        );
                        assert_eq!(
                            self.netlist[new_node_id].outputs_len(),
                            netlist[node_id].outputs_len()
                        );
                    }
                    _ => {
                        self.netlist.add(target, node);
                    }
                }
            }

            for node_out_id in netlist.mod_outputs(source) {
                let new_node_out_id = node_out_id.with_module_id(target);
                self.netlist.add_output(new_node_out_id);
            }

            let _ = res.get_or_insert(target);
        }

        Ok(res.unwrap())
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
}
