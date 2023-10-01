use std::borrow::Cow;

use ferrum_netlist::{
    group::ItemId,
    net_list::{ModuleId, NetList, NodeId},
    node::{InputNode, Pass},
    params::Outputs,
    sig_ty::{PrimTy, SignalTy},
};
use rustc_ast::{Mutability, UintTy};
use rustc_hir::{
    def::Res, BodyId, FnDecl, FnSig, ImplItem, ImplItemKind, Item, ItemKind, MutTy,
    Param, PrimTy as HirPrimTy, QPath, Ty as HirTy, TyKind as HirTyKind,
};
use rustc_middle::ty::{GenericArgsRef, TyKind};
use rustc_span::symbol::Ident;

use super::{EvalContext, Generator};
use crate::{
    blackbox,
    error::{Error, SpanError, SpanErrorKind},
    idents::Idents,
};

impl<'tcx> Generator<'tcx> {
    pub fn evaluate_fn_item(
        &mut self,
        item: &Item<'tcx>,
        generic_args: GenericArgsRef<'tcx>,
        _is_top_module: bool,
    ) -> Result<Option<ModuleId>, Error> {
        if let ItemKind::Fn(FnSig { decl, .. }, _, body_id) = item.kind {
            return self
                .evaluate_fn(item.ident.as_str(), decl, body_id, generic_args)
                .map(Some);
        }

        Ok(None)
    }

    pub fn evaluate_impl_item(
        &mut self,
        impl_item: &ImplItem<'tcx>,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Result<Option<ModuleId>, Error> {
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
        if let ImplItemKind::Fn(FnSig { decl, .. }, body_id) = impl_item.kind {
            return self
                .evaluate_fn(ident.as_ref(), decl, body_id, generic_args)
                .map(Some);
        }

        Ok(None)
    }

    pub fn evaluate_fn(
        &mut self,
        name: &str,
        fn_decl: &FnDecl<'tcx>,
        body_id: BodyId,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Result<ModuleId, Error> {
        let module_sym = self.idents.module(name);
        let module_id = self.net_list.add_module(module_sym);

        self.idents.for_module(module_id).push_scope();

        let body = self.tcx.hir().body(body_id);
        let inputs = fn_decl.inputs.iter().zip(body.params.iter());
        let ctx = EvalContext::new(generic_args, module_id);

        self.evaluate_inputs(inputs, &ctx, false, &mut |_| {})?;
        let item_id = self.evaluate_expr(body.value, &ctx)?;
        self.evaluate_outputs(item_id);

        self.idents.for_module(module_id).pop_scope();

        Ok(module_id)
    }

    pub fn evaluate_inputs<'a, F: FnMut(ItemId)>(
        &mut self,
        inputs: impl Iterator<Item = (&'a HirTy<'tcx>, &'a Param<'tcx>)>,
        ctx: &EvalContext<'tcx>,
        is_dummy: bool,
        f: &mut F,
    ) -> Result<(), Error>
    where
        'tcx: 'a,
    {
        for (input, param) in inputs {
            let ty = self.node_type(param.hir_id, ctx);
            if let TyKind::Adt(adt, ..) = ty.kind() {
                if blackbox::ignore_ty(&self.tcx.def_path(adt.did())) {
                    continue;
                }
            }

            let item_id = self.make_input(input, ctx, is_dummy)?;
            self.pattern_match(param.pat, item_id, ctx.module_id)?;

            f(item_id);
        }

        Ok(())
    }

    fn make_input(
        &mut self,
        input: &HirTy<'tcx>,
        ctx: &EvalContext<'tcx>,
        is_dummy: bool,
    ) -> Result<ItemId, Error> {
        match input.kind {
            HirTyKind::Infer => {
                let sig_ty = self.find_sig_ty(
                    self.node_type(input.hir_id, ctx),
                    ctx.generic_args,
                    input.span,
                )?;

                Ok(self.make_input_with_sig_ty(sig_ty, ctx.module_id, is_dummy))
            }
            HirTyKind::Path(QPath::Resolved(_, path)) => {
                let fn_id = input.hir_id.owner.def_id;
                let mut find_sig_ty = |def_id| {
                    self.find_sig_ty(def_id, ctx.generic_args, input.span)
                        .or_else(|_| {
                            self.find_sig_ty_for_hir_ty(
                                fn_id,
                                input,
                                ctx.generic_args,
                                input.span,
                            )
                        })
                };
                let (is_self_param, sig_ty) = match path.res {
                    Res::Def(_, def_id) => (false, find_sig_ty(def_id)?),
                    Res::SelfTyAlias { alias_to, .. } => (true, find_sig_ty(alias_to)?),
                    Res::PrimTy(HirPrimTy::Uint(UintTy::Usize | UintTy::U128)) => {
                        (false, PrimTy::U128.into())
                    }
                    _ => panic!("Cannot define def_id for {:?}", path.res),
                };

                let input = self.make_input_with_sig_ty(sig_ty, ctx.module_id, is_dummy);

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
            ) => self.make_input(ty, ctx, is_dummy),
            HirTyKind::Tup(ty) => {
                let tuple_ty = self
                    .find_sig_ty(
                        self.node_type(input.hir_id, ctx),
                        ctx.generic_args,
                        input.span,
                    )?
                    .struct_ty();

                self.make_struct_group(tuple_ty, ty.iter(), |generator, ty| {
                    generator.make_input(ty, ctx, is_dummy)
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
        module_id: ModuleId,
        is_dummy: bool,
    ) -> ItemId {
        match sig_ty {
            SignalTy::Prim(prim_ty) => {
                let input =
                    InputNode::new(prim_ty, self.idents.for_module(module_id).tmp());
                (if is_dummy {
                    self.net_list.add_dummy_node(module_id, input)
                } else {
                    self.net_list.add_node(module_id, input)
                })
                .into()
            }
            SignalTy::Array(ty) => self
                .make_array_group(ty, ty.tys(), |generator, ty| {
                    Ok(generator.make_input_with_sig_ty(ty, module_id, is_dummy))
                })
                .unwrap(),
            SignalTy::Struct(ty) => self
                .make_struct_group(
                    ty,
                    ty.tys().iter().map(|ty| ty.inner),
                    |generator, ty| {
                        Ok(generator.make_input_with_sig_ty(ty, module_id, is_dummy))
                    },
                )
                .unwrap(),
            SignalTy::Enum(ty) => {
                let input =
                    InputNode::new(ty.prim_ty(), self.idents.for_module(module_id).tmp());
                (if is_dummy {
                    self.net_list.add_dummy_node(module_id, input)
                } else {
                    self.net_list.add_node(module_id, input)
                })
                .into()
            }
        }
    }

    fn evaluate_outputs(&mut self, item_id: ItemId) {
        for node_id in item_id.into_iter() {
            Self::make_output(&mut self.net_list, &mut self.idents, node_id);
        }
    }

    fn make_output(net_list: &mut NetList, idents: &mut Idents, node_id: NodeId) {
        let module_id = node_id.module_id();
        let node = &net_list[node_id];
        let node_id = if node.is_input() {
            let out = node.outputs().only_one();
            let pass = Pass::new(
                out.out.ty,
                out.node_out_id(node_id),
                idents.for_module(module_id).tmp(),
            );

            net_list.add_node(node_id.module_id(), pass)
        } else {
            let node = &mut net_list[node_id];
            if !node.is_pass() {
                for out in node.outputs_mut().items_mut() {
                    let sym = idents.for_module(module_id).out();
                    out.out.sym = sym;
                }
            }

            node_id
        };

        net_list.add_all_outputs(node_id);
    }
}
