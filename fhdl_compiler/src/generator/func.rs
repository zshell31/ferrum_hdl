use std::borrow::Cow;

use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{
    group::ItemId,
    net_list::ModuleId,
    node::{Input, ModInst},
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
use rustc_span::{Span, Symbol as RustSymbol};
use smallvec::SmallVec;

use super::{
    expr::ExprOrItemId, metadata::resolver::MetadataResolver, Generator, MonoItem,
};
use crate::{
    blackbox::Blackbox,
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
        if self.is_inlined(body_id.hir_id.owner.def_id) {
            self.netlist[module_id].is_inlined = true;
        }

        self.idents.for_module(module_id).push_scope();

        let mut ctx = EvalContext::new(self.is_primary, generic_args, module_id);

        self.eval_inputs(inputs, &mut ctx, false)?;
        let item_id = self.eval_expr(body.value, &mut ctx)?;
        self.eval_outputs(
            self.tcx
                .hir()
                .find_parent(body_id.hir_id)
                .and_then(|node| node.ident())
                .as_ref()
                .map(|ident| ident.as_str()),
            item_id,
        );

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
                    self.idents.for_module(ctx.module_id).add_self_ident(input);
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

    fn eval_outputs(&mut self, prefix: Option<&str>, item_id: ItemId) {
        for node_out_id in item_id.into_iter() {
            let out = &mut self.netlist[node_out_id];
            if out.sym.is_none() {
                out.sym = match prefix {
                    Some(prefix) => Some(Symbol::new_from_args(format_args!(
                        "{}_{}",
                        prefix,
                        SymIdent::Out.as_str()
                    ))),
                    None => SymIdent::Out.into(),
                };
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

        Ok(self.instantiate_module(module_id, args, output_ty, ctx))
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

        Ok(self.instantiate_module(module_id, inputs, output_ty, ctx))
    }

    fn instantiate_module(
        &mut self,
        instant_mod_id: ModuleId,
        inputs: SmallVec<[ItemId; 4]>,
        sig_ty: SignalTy,
        ctx: &EvalContext<'tcx>,
    ) -> ItemId {
        let inputs = inputs.into_iter().flat_map(|input| input.into_iter());

        let outputs = self
            .netlist
            .mod_outputs(instant_mod_id)
            .map(|node_out_id| (self.netlist[node_out_id].ty, None));

        let mod_inst = ModInst::new(None, instant_mod_id, inputs, outputs);
        let node_id = self.netlist.add(ctx.module_id, mod_inst);

        self.combine_node_outputs(node_id, sig_ty)
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
            let mono_item = MonoItem::new(fn_did, fn_generics);

            #[allow(clippy::map_entry)]
            if !self.evaluated_modules.contains_key(&mono_item) {
                // resolve all types from the imported module using fn_generics
                let ctx = ctx.with_generic_args(fn_generics);
                let mut resolver =
                    MetadataResolver::new(self, &metadata, fn_did, &ctx, span);
                let module_id = resolver.import_module_from_metadata(module_id)?;

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

        Ok(self.instantiate_module(module_id, inputs, output_ty, ctx))
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

    pub fn eval_closure_as_module(
        &mut self,
        name: Symbol,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ModuleId, Error> {
        println!("{:#?}", self.idents);
        let module_id = self.netlist.add_module(name, false);
        let item_id =
            ctx.eval_closure_as_module(module_id, |ctx| self.eval_expr(expr, ctx))?;
        self.eval_outputs(None, item_id);

        Ok(module_id)
    }
}
