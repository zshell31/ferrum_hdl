use std::borrow::Cow;

use fhdl_netlist::{
    group::ItemId,
    net_list::ModuleId,
    node::Input,
    sig_ty::{NodeTy, SignalTy, SignalTyKind},
    symbol::Symbol,
};
use rustc_ast::{Mutability, UintTy};
use rustc_hir::{
    def::Res, BodyId, FnDecl, FnSig, ImplItem, ImplItemKind, Item, ItemKind, MutTy,
    Param, PrimTy as HirPrimTy, QPath, Ty as HirTy, TyKind as HirTyKind,
};
use rustc_middle::ty::{GenericArgsRef, TyKind};
use rustc_span::symbol::Ident;

use super::Generator;
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    scopes::SymIdent,
};

impl<'tcx> Generator<'tcx> {
    pub fn eval_fn_item(
        &mut self,
        item: &Item<'tcx>,
        top_module: bool,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Result<Option<ModuleId>, Error> {
        if let ItemKind::Fn(FnSig { decl, .. }, _, body_id) = item.kind {
            return self
                .eval_fn(item.ident.as_str(), decl, body_id, top_module, generic_args)
                .map(Some);
        }

        Ok(None)
    }

    pub fn eval_impl_item(
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
                .eval_fn(ident.as_ref(), decl, body_id, false, generic_args)
                .map(Some);
        }

        Ok(None)
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
        let module_id = self.net_list.add_module(module_sym, top_module);

        self.idents.for_module(module_id).push_scope();

        let mut ctx = EvalContext::new(generic_args, module_id);

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
                let sig_ty = self.find_sig_ty(
                    self.node_type(input.hir_id, ctx),
                    ctx.generic_args,
                    input.span,
                )?;

                Ok(self.make_input_with_sig_ty(sig_ty, ctx, is_closure))
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
                    Res::PrimTy(HirPrimTy::Bool) => {
                        (false, SignalTy::new(None, NodeTy::Bool.into()))
                    }
                    Res::PrimTy(HirPrimTy::Uint(UintTy::U8)) => {
                        (false, SignalTy::new(None, NodeTy::U8.into()))
                    }
                    Res::PrimTy(HirPrimTy::Uint(UintTy::U16)) => {
                        (false, SignalTy::new(None, NodeTy::U16.into()))
                    }
                    Res::PrimTy(HirPrimTy::Uint(UintTy::U32)) => {
                        (false, SignalTy::new(None, NodeTy::U32.into()))
                    }
                    Res::PrimTy(HirPrimTy::Uint(UintTy::U64)) => {
                        (false, SignalTy::new(None, NodeTy::U64.into()))
                    }
                    Res::PrimTy(HirPrimTy::Uint(UintTy::U128)) => {
                        (false, SignalTy::new(None, NodeTy::U128.into()))
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
                    .find_sig_ty(
                        self.node_type(input.hir_id, ctx),
                        ctx.generic_args,
                        input.span,
                    )?
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
                self.net_list.add_and_get_out(module_id, input)
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
                self.net_list.add_and_get_out(module_id, input)
            })
            .into(),
        }
    }

    fn eval_outputs(&mut self, item_id: ItemId) {
        for node_out_id in item_id.into_iter() {
            let out = &mut self.net_list[node_out_id];
            if out.sym.is_none() {
                out.sym = SymIdent::Out.into();
            }
            self.net_list.add_output(node_out_id);
        }
    }
}
