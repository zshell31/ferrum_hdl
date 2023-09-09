use std::iter;

use ferrum_netlist::{
    group_list::{Group, GroupKind, ItemId},
    net_list::{ModuleId, NetList, NodeId, NodeOutId},
    node::{InputNode, IsNode, PassNode, Splitter},
    params::Outputs,
    sig_ty::{PrimTy, SignalTy},
};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::{
    def_id::{DefId, LocalDefId},
    BodyId, FnDecl, FnSig, Generics as HirGenerics, Item, ItemKind, Param, Pat, PatKind,
    QPath, Ty as HirTy, TyKind as HirTyKind, WherePredicate,
};
use rustc_middle::ty::{GenericArg, List};
use rustc_span::{symbol::Ident, Span};

use super::{AsKey, EvalContext, Generator};
use crate::{
    bitvec::ArrayDesc,
    blackbox::ItemPath,
    error::{Error, SpanError, SpanErrorKind},
    idents::Idents,
    utils,
};

impl<'tcx> Generator<'tcx> {
    pub fn evaluate_item(
        &mut self,
        item: &Item<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        is_top_module: bool,
    ) -> Result<Option<ModuleId>, Error> {
        if let ItemKind::Fn(FnSig { decl, .. }, hir_generics, body_id) = item.kind {
            let fn_id = item.hir_id().owner.def_id;
            // ignore unsupported generics if current item is not top module
            self.evaluate_generics(fn_id, hir_generics, generics, !is_top_module)?;

            return self
                .evaluate_fn(item.ident, decl, body_id, generics)
                .map(Some);
        }

        Ok(None)
    }

    fn evaluate_generics(
        &mut self,
        fn_id: LocalDefId,
        hir_generics: &HirGenerics<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        ignore: bool,
    ) -> Result<(), Error> {
        let make_err =
            |span| Error::from(SpanError::new(SpanErrorKind::UnsupportedGeneric, span));

        let mut params: FxHashMap<DefId, Span> = hir_generics
            .params
            .iter()
            .map(|param| (param.def_id.to_def_id(), param.span))
            .collect();

        for predicate in hir_generics.predicates {
            match predicate {
                WherePredicate::BoundPredicate(predicate) => {
                    let def_id = match predicate.bounded_ty.as_generic_param() {
                        Some((def_id, _)) => def_id,
                        None => {
                            continue;
                        }
                    };

                    let span = match params.remove_entry(&def_id) {
                        Some((_, span)) => span,
                        None => {
                            continue;
                        }
                    };

                    let mut found_signal = false;
                    for bound in predicate.bounds {
                        let trait_ref = match bound.trait_ref() {
                            Some(trait_ref) => trait_ref,
                            None => {
                                continue;
                            }
                        };

                        let trait_def_id = match trait_ref.path.res.opt_def_id() {
                            Some(def_id) => def_id,
                            None => {
                                continue;
                            }
                        };

                        // TODO: move into blackbox
                        if self.tcx.def_path(trait_def_id)
                            == ItemPath(&["signal", "Signal"])
                        {
                            let arg = trait_ref.path.segments[0]
                                .args
                                .ok_or_else(|| make_err(span))?;

                            let ty = arg.bindings[0].ty();
                            let sig_ty =
                                self.find_sig_ty_for_hir_ty(fn_id, ty, generics, span)?;

                            let key = def_id.as_key(self, generics, span)?;
                            self.sig_ty.insert(key, Some(sig_ty));

                            let key = self
                                .ast_ty_to_ty(fn_id, predicate.bounded_ty)
                                .as_key(self, generics, span)?;
                            self.sig_ty.insert(key, Some(sig_ty));

                            found_signal = true;

                            break;
                        }
                    }

                    if !ignore && !found_signal {
                        return Err(make_err(span));
                    }
                }
                _ => {
                    if !ignore {
                        return Err(make_err(predicate.span()));
                    }
                }
            }
        }

        if !ignore && !params.is_empty() {
            let span = params.values().next().unwrap();
            return Err(make_err(*span));
        }

        Ok(())
    }

    pub fn evaluate_fn(
        &mut self,
        name: Ident,
        fn_decl: &FnDecl<'tcx>,
        body_id: BodyId,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
    ) -> Result<ModuleId, Error> {
        let module_sym = self.idents.module(name);
        let module_id = self.net_list.add_module(module_sym);

        self.idents.for_module(module_id).push_scope();

        let body = self.tcx.hir().body(body_id);
        let inputs = fn_decl.inputs.iter().zip(body.params.iter());
        let ctx = EvalContext::new(generics, module_id);

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
                        let sym = self.idents.for_module(module_id).ident(ident);
                        self.net_list[node_id].outputs_mut().only_one_mut().out.sym = sym;
                    }
                    ItemId::Group(group_id) => {
                        let item_ids = self.group_list[group_id].item_ids;
                        for item_id in item_ids.iter() {
                            self.pattern_match(pat, *item_id, module_id)?;
                        }
                    }
                }

                self.idents
                    .for_module(module_id)
                    .add_local_ident(ident, item_id);
            }
            PatKind::Slice(before, wild, after) => {
                let ArrayDesc { count, width } =
                    self.opt_array_desc(item_id).ok_or_else(|| {
                        SpanError::new(SpanErrorKind::ExpectedArray, pat.span)
                    })?;

                let sig_ty =
                    self.item_ty(self.group_list[item_id.group_id()].item_ids[0]);

                let to = self.to_bitvec(module_id, item_id);

                self.slice_pattern_match(to, width, sig_ty, before, None)?;

                if wild.is_some() {
                    let start = ((count - after.len()) as u128) * width;
                    self.slice_pattern_match(to, width, sig_ty, after, Some(start))?;
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
                            self.pattern_match(pat, *item_id, module_id)?;
                        }

                        for (pat, item_id) in pats[pos ..]
                            .iter()
                            .zip(item_ids[(len - (pats.len() - pos)) ..].iter())
                        {
                            self.pattern_match(pat, *item_id, module_id)?;
                        }
                    }
                    None => {
                        let group = &self.group_list[group_id];
                        assert_eq!(pats.len(), group.len());

                        let item_ids = group.item_ids;
                        for (pat, item_id) in pats.iter().zip(item_ids.iter()) {
                            self.pattern_match(pat, *item_id, module_id)?;
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
                    &self.node_type(input.hir_id),
                    ctx.generics,
                    input.span,
                )?;

                Ok(self.make_input_with_sig_ty(sig_ty, ctx.module_id, is_dummy))
            }
            HirTyKind::Path(QPath::Resolved(_, path)) => {
                let fn_id = input.hir_id.owner.def_id;
                let sig_ty = self
                    .find_sig_ty(&path.res.def_id(), ctx.generics, input.span)
                    .or_else(|_| {
                        self.find_sig_ty_for_hir_ty(
                            fn_id,
                            input,
                            ctx.generics,
                            input.span,
                        )
                    })?;

                Ok(self.make_input_with_sig_ty(sig_ty, ctx.module_id, is_dummy))
            }
            HirTyKind::Tup(ty) => {
                let group = ty
                    .iter()
                    .map(|ty| self.make_input(ty, ctx, is_dummy))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(self
                    .group_list
                    .add_group(Group::new(GroupKind::Group, group))
                    .into())
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
            SignalTy::Array(n, ty) => {
                let group = Group::new(
                    sig_ty.into(),
                    iter::repeat(ty).take(n as usize).map(|sig_ty| {
                        self.make_input_with_sig_ty(*sig_ty, module_id, is_dummy)
                    }),
                );

                self.group_list.add_group(group).into()
            }
            SignalTy::Group(ty) => {
                let group = Group::new(
                    sig_ty.into(),
                    ty.iter().map(|sig_ty| {
                        self.make_input_with_sig_ty(*sig_ty, module_id, is_dummy)
                    }),
                );
                self.group_list.add_group(group).into()
            }
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
