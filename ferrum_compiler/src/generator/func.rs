use std::{borrow::Cow, iter};

use ferrum_netlist::{
    group_list::ItemId,
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{InputNode, IsNode, PassNode, Splitter},
    params::Outputs,
    sig_ty::{ArrayTy, PrimTy, SignalTy},
};
use rustc_ast::Mutability;
use rustc_hir::{
    def::Res, BodyId, FnDecl, FnSig, ImplItem, ImplItemKind, Item, ItemKind, MutTy,
    Param, Pat, PatKind, QPath, Ty as HirTy, TyKind as HirTyKind,
};
use rustc_middle::ty::GenericArgsRef;
use rustc_span::symbol::Ident;

use super::{EvalContext, Generator};
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    idents::Idents,
    utils,
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

        self.evaluate_inputs(inputs, ctx, false, &mut |_| {})?;
        let item_id = self.evaluate_expr(body.value, ctx)?;
        self.evaluate_outputs(item_id)?;

        self.idents.for_module(module_id).pop_scope();

        Ok(module_id)
    }

    pub fn evaluate_inputs<'a, F: FnMut(ItemId)>(
        &mut self,
        inputs: impl Iterator<Item = (&'a HirTy<'tcx>, &'a Param<'tcx>)>,
        ctx: EvalContext<'tcx>,
        is_dummy: bool,
        f: &mut F,
    ) -> Result<(), Error>
    where
        'tcx: 'a,
    {
        for (input, param) in inputs {
            let item_id = self.make_input(input, ctx, is_dummy)?;
            self.pattern_match(param.pat, item_id, ctx.module_id)?;

            f(item_id);
        }

        Ok(())
    }

    pub fn pattern_match(
        &mut self,
        pat: &Pat<'tcx>,
        item_id: ItemId,
        module_id: ModuleId,
    ) -> Result<(), Error> {
        match pat.kind {
            PatKind::Binding(..) => {
                let ident = utils::pat_ident(pat)?;
                match item_id {
                    ItemId::Node(node_id) => {
                        let sym = self.idents.for_module(module_id).ident(ident.as_str());
                        self.net_list[node_id].outputs_mut().only_one_mut().out.sym = sym;
                    }
                    ItemId::Group(group_id) => {
                        let item_ids = self.group_list[group_id].item_ids;
                        for item_id in item_ids {
                            self.pattern_match(pat, item_id.inner, module_id)?;
                        }
                    }
                }

                self.idents
                    .for_module(module_id)
                    .add_local_ident(ident, item_id);
            }
            PatKind::Slice(before, wild, after) => {
                let ArrayTy(count, sig_ty) =
                    self.item_ty(item_id).opt_array_ty().ok_or_else(|| {
                        SpanError::new(SpanErrorKind::ExpectedArray, pat.span)
                    })?;
                let width = sig_ty.width();

                let to = self.to_bitvec(module_id, item_id);

                self.slice_pattern_match(to, width, *sig_ty, before, None)?;

                if wild.is_some() {
                    let start = (count - (after.len() as u128)) * width;
                    self.slice_pattern_match(to, width, *sig_ty, after, Some(start))?;
                }
            }
            PatKind::Tuple(pats, dot_dot_pos) => {
                let group_id = item_id.group_id();
                match dot_dot_pos.as_opt_usize() {
                    Some(pos) => {
                        let group = &self.group_list[group_id];
                        let len = group.len();
                        assert!(pats.len() < len);

                        let item_ids = group.item_ids;
                        for (pat, item_id) in
                            pats[0 .. pos].iter().zip(item_ids[0 .. pos].iter())
                        {
                            self.pattern_match(pat, item_id.inner, module_id)?;
                        }

                        for (pat, item_id) in pats[pos ..]
                            .iter()
                            .zip(item_ids[(len - (pats.len() - pos)) ..].iter())
                        {
                            self.pattern_match(pat, item_id.inner, module_id)?;
                        }
                    }
                    None => {
                        let group = &self.group_list[group_id];
                        assert_eq!(pats.len(), group.len());

                        let item_ids = group.item_ids;
                        for (pat, item_id) in pats.iter().zip(item_ids.iter()) {
                            self.pattern_match(pat, item_id.inner, module_id)?;
                        }
                    }
                }
            }
            PatKind::Wild => {}
            _ => {
                println!("{:#?}", pat);
                return Err(
                    SpanError::new(SpanErrorKind::ExpectedIdentifier, pat.span).into()
                );
            }
        }

        Ok(())
    }

    fn slice_pattern_match(
        &mut self,
        node_out_id: NodeOutId,
        width: u128,
        sig_ty: SignalTy,
        pat: &[Pat<'tcx>],
        start: Option<u128>,
    ) -> Result<(), Error> {
        if pat.is_empty() {
            return Ok(());
        }

        let module_id = node_out_id.node_id().module_id();
        let splitter = Splitter::new(
            node_out_id,
            (0 .. pat.len()).map(|_| {
                (
                    PrimTy::BitVec(width),
                    self.idents.for_module(module_id).tmp(),
                )
            }),
            start,
        );
        let node_id = self.net_list.add_node(module_id, splitter);

        let outputs = self.net_list[node_id].outputs();
        assert_eq!(outputs.len(), pat.len());

        let node_out_ids = outputs
            .items()
            .map(|out| out.node_out_id(node_id))
            .collect::<Vec<_>>();

        for (node_out_id, before_pat) in node_out_ids.into_iter().zip(pat) {
            let from = self.from_bitvec(module_id, node_out_id, sig_ty);
            self.pattern_match(before_pat, from, module_id)?;
        }

        Ok(())
    }

    fn make_input(
        &mut self,
        input: &HirTy<'tcx>,
        ctx: EvalContext<'tcx>,
        is_dummy: bool,
    ) -> Result<ItemId, Error> {
        match input.kind {
            HirTyKind::Infer => {
                let sig_ty = self.find_sig_ty(
                    self.node_type(input.hir_id, ctx.generic_args),
                    ctx.generic_args,
                    input.span,
                )?;

                Ok(self.make_input_with_sig_ty(sig_ty, ctx.module_id, is_dummy))
            }
            HirTyKind::Path(QPath::Resolved(_, path)) => {
                let fn_id = input.hir_id.owner.def_id;
                let (is_self_param, def_id) = match path.res {
                    Res::Def(_, def_id) => (false, def_id),
                    Res::SelfTyAlias { alias_to, .. } => (true, alias_to),
                    _ => panic!("Cannot define def_id for {:?}", path.res),
                };

                let sig_ty = self
                    .find_sig_ty(def_id, ctx.generic_args, input.span)
                    .or_else(|_| {
                        self.find_sig_ty_for_hir_ty(
                            fn_id,
                            input,
                            ctx.generic_args,
                            input.span,
                        )
                    })?;

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
            HirTyKind::Tup(ty) => self.make_tuple_group(ty.iter(), |generator, ty| {
                generator.make_input(ty, ctx, is_dummy)
            }),
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
            SignalTy::Array(ArrayTy(n, ty)) => self
                .make_array_group(iter::repeat(ty).take(n as usize), |generator, ty| {
                    Ok(generator.make_input_with_sig_ty(*ty, module_id, is_dummy))
                })
                .unwrap(),
            SignalTy::Group(ty) => self
                .make_struct_group(
                    ty.iter().map(|ty| (ty.name, ty.inner)),
                    |generator, ty| {
                        Ok(generator.make_input_with_sig_ty(ty, module_id, is_dummy))
                    },
                )
                .unwrap(),
        }
    }

    fn evaluate_outputs(&mut self, item_id: ItemId) -> Result<(), Error> {
        self.group_list
            .deep_iter::<Error, _>(&[item_id], &mut |_, node_id| {
                Self::make_output(&mut self.net_list, &mut self.idents, node_id);

                Ok(())
            })
    }

    fn make_output(net_list: &mut NetList, idents: &mut Idents, node_id: NodeId) {
        let module_id = node_id.module_id();
        let node = &net_list[node_id];
        let node_id = if node.is_input() || node.is_pass() {
            let out = node.outputs().only_one();
            let mut pass = PassNode::new(
                out.out.ty,
                out.node_out_id(node_id),
                idents.for_module(module_id).tmp(),
            );
            pass.inject = Some(false);

            net_list.add_node(node_id.module_id(), pass)
        } else {
            node_id
        };

        let node = &mut net_list[node_id];
        for out in node.outputs_mut().items_mut() {
            let sym = idents.for_module(module_id).out();
            out.out.sym = sym;
        }

        net_list.add_all_outputs(node_id);
    }
}
