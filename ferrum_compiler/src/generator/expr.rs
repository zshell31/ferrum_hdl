use std::{
    iter,
    sync::atomic::{AtomicU8, Ordering},
};

use ferrum_netlist::{
    arena::with_arena,
    group_list::{Group, GroupKind, ItemId, Named},
    net_list::{ModuleId, NodeId, NodeOutId},
    node::{
        BinOp, BinOpNode, BitNotNode, ConstNode, InputNode, IsNode, ModInst, Mux2Node,
        Node, NotNode, PassNode, Splitter,
    },
    params::Outputs,
    sig_ty::{ArrayTy, PrimTy, SignalTy},
    symbol::Symbol,
};
use rustc_ast::LitKind;
use rustc_hir::{
    def::{DefKind, Res},
    def_id::{DefId, LocalDefId},
    BinOpKind, Expr, ExprKind, Node as HirNode, Path, QPath, StmtKind, UnOp,
};
use rustc_middle::ty::{FnSig, GenericArgs, GenericArgsRef};
use rustc_span::{source_map::Spanned, symbol::Ident, Span};
use smallvec::SmallVec;

use super::{
    arg_matcher::ArgMatcher, EvalContext, Generator, MonoItem, TraitImpls, TraitKind,
};
use crate::{
    blackbox::{self, bit_vec_trans, Blackbox, EvaluateExpr, StdConversion},
    error::{Error, SpanError, SpanErrorKind},
    utils,
};

static IDENT: AtomicU8 = AtomicU8::new(0);

struct Scope<'tcx> {
    module: Symbol,
    kind: &'tcx ExprKind<'tcx>,
    generic_args: GenericArgsRef<'tcx>,
}

impl<'tcx> Scope<'tcx> {
    fn enter(
        module: Symbol,
        kind: &'tcx ExprKind<'tcx>,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Self {
        let ident = AtomicU8::fetch_add(&IDENT, 1, Ordering::Relaxed) as usize;
        println!(
            "{}--> {} ({} <{:?}>)",
            " ".repeat(ident << 2),
            Self::to_str(kind),
            module,
            generic_args
        );

        Self {
            module,
            kind,
            generic_args,
        }
    }

    fn exit(self) {
        let ident = AtomicU8::fetch_sub(&IDENT, 1, Ordering::Relaxed) as usize;
        println!(
            "{}<-- {} ({} <{:?}>)",
            " ".repeat((ident - 1) << 2),
            Self::to_str(self.kind),
            self.module,
            self.generic_args
        );
    }

    fn to_str(kind: &'tcx ExprKind<'tcx>) -> &'static str {
        match kind {
            ExprKind::ConstBlock(_) => "const block",
            ExprKind::Array(_) => "array",
            ExprKind::Call(_, _) => "call",
            ExprKind::MethodCall(_, _, _, _) => "method call",
            ExprKind::Tup(_) => "tup",
            ExprKind::Binary(_, _, _) => "binary",
            ExprKind::Unary(_, _) => "unary",
            ExprKind::Lit(_) => "lit",
            ExprKind::Cast(_, _) => "cast",
            ExprKind::Type(_, _) => "type",
            ExprKind::DropTemps(_) => "drop temps",
            ExprKind::Let(_) => "let",
            ExprKind::If(_, _, _) => "if",
            ExprKind::Loop(_, _, _, _) => "loop",
            ExprKind::Match(_, _, _) => "match",
            ExprKind::Closure(_) => "closure",
            ExprKind::Block(_, _) => "block",
            ExprKind::Assign(_, _, _) => "assign",
            ExprKind::AssignOp(_, _, _) => "assign op",
            ExprKind::Field(_, _) => "field",
            ExprKind::Index(_, _, _) => "index",
            ExprKind::Path(_) => "path",
            ExprKind::AddrOf(_, _, _) => "addr of",
            ExprKind::Break(_, _) => "break",
            ExprKind::Continue(_) => "continue",
            ExprKind::Ret(_) => "ret",
            ExprKind::Become(_) => "become",
            ExprKind::InlineAsm(_) => "inline asm",
            ExprKind::OffsetOf(_, _) => "offset of",
            ExprKind::Struct(_, _, _) => "struct",
            ExprKind::Repeat(_, _) => "repeat",
            ExprKind::Yield(_, _) => "yield",
            ExprKind::Err(_) => "err",
        }
    }
}

pub enum ExprOrItemId<'tcx> {
    Expr(&'tcx Expr<'tcx>),
    Item(ItemId),
}

impl<'tcx> From<&'tcx Expr<'tcx>> for ExprOrItemId<'tcx> {
    fn from(expr: &'tcx Expr<'tcx>) -> Self {
        ExprOrItemId::Expr(expr)
    }
}

impl<'tcx> From<ItemId> for ExprOrItemId<'tcx> {
    fn from(item_id: ItemId) -> Self {
        ExprOrItemId::Item(item_id)
    }
}

impl<'tcx> ExprOrItemId<'tcx> {
    pub fn evaluate(
        &self,
        generator: &mut Generator<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        match self {
            ExprOrItemId::Expr(expr) => generator.evaluate_expr(expr, ctx),
            ExprOrItemId::Item(item_id) => Ok(*item_id),
        }
    }
}

impl<'tcx> Generator<'tcx> {
    pub fn make_tuple_group<T>(
        &mut self,
        iter: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<ItemId, Error>,
    ) -> Result<ItemId, Error> {
        let group = unsafe {
            with_arena().alloc_from_res_iter(iter.into_iter().enumerate().map(
                |(ind, item)| {
                    f(self, item).map(|item_id| {
                        Named::new(item_id, Some(Symbol::new_from_ind(ind)))
                    })
                },
            ))?
        };

        Ok(self
            .group_list
            .add_group(Group::new_with_item_ids(GroupKind::Group, group))
            .into())
    }

    pub fn make_tuple_sig_ty<T>(
        &mut self,
        iter: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<SignalTy, Error>,
    ) -> Result<SignalTy, Error> {
        let ty = unsafe {
            with_arena().alloc_from_res_iter(iter.into_iter().enumerate().map(
                |(ind, item)| {
                    f(self, item)
                        .map(|sig_ty| Named::new(sig_ty, Some(Symbol::new_from_ind(ind))))
                },
            ))?
        };

        Ok(SignalTy::Group(ty))
    }

    pub fn make_array_group<T>(
        &mut self,
        iter: impl IntoIterator<Item = T>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<ItemId, Error>,
    ) -> Result<ItemId, Error> {
        let group = unsafe {
            with_arena().alloc_from_res_iter(
                iter.into_iter()
                    .map(|item| f(self, item).map(|item_id| Named::new(item_id, None))),
            )?
        };

        Ok(self
            .group_list
            .add_group(Group::new_with_item_ids(GroupKind::Array, group))
            .into())
    }

    pub fn make_struct_group<T>(
        &mut self,
        iter: impl IntoIterator<Item = (Option<Symbol>, T)>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<ItemId, Error>,
    ) -> Result<ItemId, Error> {
        let group = unsafe {
            with_arena().alloc_from_res_iter(iter.into_iter().map(|(sym, item)| {
                f(self, item).map(|item_id| Named::new(item_id, sym))
            }))?
        };

        Ok(self
            .group_list
            .add_group(Group::new_with_item_ids(GroupKind::Group, group))
            .into())
    }

    pub fn make_struct_sig_ty<T>(
        &mut self,
        iter: impl IntoIterator<Item = (Ident, T)>,
        mut f: impl FnMut(&mut Generator<'tcx>, T) -> Result<SignalTy, Error>,
    ) -> Result<SignalTy, Error> {
        let ty = unsafe {
            with_arena().alloc_from_res_iter(iter.into_iter().map(
                |(ident, item): (Ident, T)| {
                    f(self, item).map(|sig_ty| {
                        Named::new(sig_ty, Some(Symbol::new(ident.as_str())))
                    })
                },
            ))?
        };

        Ok(SignalTy::Group(ty))
    }

    pub fn make_struct_group_from_exprs(
        &mut self,
        expr: &Expr<'tcx>,
        sub_exprs: impl IntoIterator<Item = &'tcx Expr<'tcx>> + 'tcx,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let sig_ty = self.find_sig_ty(
            self.node_type(expr.hir_id, ctx.generic_args),
            ctx.generic_args,
            expr.span,
        )?;
        match sig_ty {
            SignalTy::Group(ty) => {
                let sub_exprs = sub_exprs
                    .into_iter()
                    .filter(|sub_expr| {
                        let ty = self.node_type(sub_expr.hir_id, ctx.generic_args);
                        match utils::ty_def_id(ty) {
                            Some(def_id) => {
                                !blackbox::ignore_ty(&self.tcx.def_path(def_id))
                            }
                            None => true,
                        }
                    })
                    .collect::<Vec<_>>();

                assert_eq!(sub_exprs.len(), ty.len());

                self.make_struct_group(
                    ty.iter()
                        .zip(sub_exprs)
                        .map(|(ty, sub_expr)| (ty.name, sub_expr)),
                    |generator, sub_expr| generator.evaluate_expr(sub_expr, ctx),
                )
            }
            _ => Err(SpanError::new(SpanErrorKind::ExpectedStructType, expr.span).into()),
        }
    }

    pub fn make_dummy_inputs_from_sig_ty(
        &mut self,
        sig_ty: SignalTy,
        ctx: EvalContext<'tcx>,
    ) -> (ItemId, SmallVec<[NodeId; 8]>) {
        let mut dummy_inputs = SmallVec::new();
        let item_id =
            self.make_dummy_inputs_from_sig_ty_inner(sig_ty, ctx, &mut dummy_inputs);
        (item_id, dummy_inputs)
    }

    fn make_dummy_inputs_from_sig_ty_inner(
        &mut self,
        sig_ty: SignalTy,
        ctx: EvalContext<'tcx>,
        dummy_inputs: &mut SmallVec<[NodeId; 8]>,
    ) -> ItemId {
        let module_id = ctx.module_id;

        match sig_ty {
            SignalTy::Prim(prim_ty) => {
                let dummy_input = self.net_list.add_dummy_node(
                    module_id,
                    InputNode::new(prim_ty, self.idents.for_module(module_id).tmp()),
                );
                dummy_inputs.push(dummy_input);
                dummy_input.into()
            }
            SignalTy::Array(ArrayTy(n, sig_ty)) => self
                .make_array_group(iter::repeat(n), |generator, _| {
                    Ok(generator.make_dummy_inputs_from_sig_ty_inner(
                        *sig_ty,
                        ctx,
                        dummy_inputs,
                    ))
                })
                .unwrap(),
            SignalTy::Group(tys) => self
                .make_struct_group(
                    tys.iter().map(|ty| (ty.name, ty.inner)),
                    |generator, sig_ty| {
                        Ok(generator.make_dummy_inputs_from_sig_ty_inner(
                            sig_ty,
                            ctx,
                            dummy_inputs,
                        ))
                    },
                )
                .unwrap(),
        }
    }

    pub fn evaluate_expr(
        &mut self,
        expr: &'tcx Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let scope = Scope::enter(
            self.net_list[ctx.module_id].name,
            &expr.kind,
            ctx.generic_args,
        );

        let ty = self.node_type(expr.hir_id, ctx.generic_args);

        let res = match expr.kind {
            ExprKind::Array(items) => self
                .make_array_group(items.iter(), |generator, item| {
                    generator.evaluate_expr(item, ctx)
                }),
            ExprKind::Binary(bin_op, lhs, rhs) => {
                let prim_ty =
                    self.find_sig_ty(ty, ctx.generic_args, expr.span)?.prim_ty();

                let bin_op = match bin_op.node {
                    BinOpKind::BitAnd => BinOp::BitAnd,
                    BinOpKind::BitOr => BinOp::BitOr,
                    BinOpKind::BitXor => BinOp::BitXor,
                    BinOpKind::And => BinOp::And,
                    BinOpKind::Or => BinOp::Or,
                    BinOpKind::Add => BinOp::Add,
                    BinOpKind::Sub => BinOp::Sub,
                    _ => {
                        return Err(SpanError::new(
                            SpanErrorKind::UnsupportedBinOp(bin_op.node),
                            bin_op.span,
                        )
                        .into());
                    }
                };

                let lhs = self.evaluate_expr(lhs, ctx)?.node_id();
                if let Node::Const(node) = &mut self.net_list[lhs] {
                    node.output.ty = prim_ty;
                    node.skip = true;
                }

                let rhs = self.evaluate_expr(rhs, ctx)?.node_id();
                if let Node::Const(node) = &mut self.net_list[rhs] {
                    node.output.ty = prim_ty;
                    node.skip = true;
                }

                let lhs = self.net_list.only_one_node_out_id(lhs);
                let rhs = self.net_list.only_one_node_out_id(rhs);

                Ok(self
                    .net_list
                    .add_node(
                        ctx.module_id,
                        BinOpNode::new(
                            prim_ty,
                            bin_op,
                            lhs,
                            rhs,
                            self.idents.for_module(ctx.module_id).tmp(),
                        ),
                    )
                    .into())
            }
            ExprKind::Block(block, _) => {
                self.idents.for_module(ctx.module_id).push_scope();

                for stmt in block.stmts {
                    match stmt.kind {
                        StmtKind::Local(local) if local.els.is_none() => {
                            let init = local.init.ok_or_else(|| {
                                SpanError::new(SpanErrorKind::ExpectedExpr, local.span)
                            })?;
                            let item_id = self.evaluate_expr(init, ctx)?;

                            let item_id = match item_id {
                                ItemId::Node(node_id) => {
                                    let outputs_len =
                                        self.net_list[node_id].outputs().len();

                                    if outputs_len > 1 {
                                        // TODO: how to avoid allocating vec
                                        let nodes = self.net_list[node_id]
                                            .outputs()
                                            .items()
                                            .map(|out| {
                                                PassNode::new(
                                                    out.out.ty,
                                                    out.node_out_id(node_id),
                                                    self.idents
                                                        .for_module(ctx.module_id)
                                                        .tmp(),
                                                )
                                            })
                                            .collect::<Vec<_>>();

                                        self.make_tuple_group(
                                            nodes,
                                            |generator, node| {
                                                Ok(generator
                                                    .net_list
                                                    .add_node(ctx.module_id, node)
                                                    .into())
                                            },
                                        )?
                                    } else {
                                        item_id
                                    }
                                }
                                ItemId::Group(_) => item_id,
                            };

                            let item_id = match item_id {
                                ItemId::Node(node_id) => {
                                    let out =
                                        &self.net_list[node_id].outputs().only_one();
                                    self.net_list
                                        .add_node(
                                            ctx.module_id,
                                            PassNode::new(
                                                out.out.ty,
                                                out.node_out_id(node_id),
                                                self.idents
                                                    .for_module(ctx.module_id)
                                                    .tmp(),
                                            ),
                                        )
                                        .into()
                                }
                                ItemId::Group(_) => item_id,
                            };

                            self.pattern_match(local.pat, item_id, ctx.module_id)?;
                        }
                        _ => {
                            return Err(SpanError::new(
                                SpanErrorKind::ExpectedLetBind,
                                stmt.span,
                            )
                            .into());
                        }
                    }
                }

                let expr = match block.expr {
                    Some(expr) => expr,
                    None => {
                        return Err(SpanError::new(
                            SpanErrorKind::ExpectedLastExpr,
                            block.span,
                        )
                        .into());
                    }
                };

                let item_id = self.evaluate_expr(expr, ctx)?;

                self.idents.for_module(ctx.module_id).pop_scope();

                Ok(item_id)
            }
            ExprKind::Call(fn_item, args) => {
                if let ExprKind::Path(path) = fn_item.kind {
                    match path {
                        QPath::Resolved(
                            _,
                            Path {
                                res: Res::SelfCtor(_),
                                ..
                            },
                        ) => self.make_struct_group_from_exprs(expr, args, ctx),
                        QPath::Resolved(_, Path { span, res, .. })
                            if res.opt_def_id().is_some() =>
                        {
                            let fn_id = res.def_id();
                            if fn_id.is_local() {
                                let fn_ty =
                                    self.node_type(fn_item.hir_id, ctx.generic_args);
                                let generic_args = self
                                    .subst_with(utils::subst(fn_ty), ctx.generic_args);

                                self.evaluate_fn_call(
                                    fn_id.expect_local(),
                                    generic_args,
                                    args.iter().map(Into::into),
                                    ctx,
                                )
                            } else {
                                let blackbox =
                                    self.find_blackbox(fn_id, ctx.generic_args, *span)?;
                                blackbox.evaluate_expr(self, expr, ctx)
                            }
                        }
                        QPath::TypeRelative(_, _) => {
                            let fn_ty = self.node_type(fn_item.hir_id, ctx.generic_args);
                            let fn_id = utils::ty_def_id(fn_ty).unwrap();
                            let generic_args =
                                self.subst_with(utils::subst(fn_ty), ctx.generic_args);

                            match self.find_local_impl_id(
                                fn_id,
                                generic_args,
                                fn_item.span,
                            )? {
                                Some((impl_id, generic_args)) => self
                                    .evaluate_impl_fn_call(
                                        impl_id,
                                        generic_args,
                                        None,
                                        args.iter().map(Into::into),
                                        ctx,
                                    ),
                                None => {
                                    let blackbox = self.find_blackbox(
                                        fn_id,
                                        ctx.generic_args,
                                        fn_item.span,
                                    )?;
                                    blackbox.evaluate_expr(self, expr, ctx)
                                }
                            }
                        }
                        _ => {
                            println!("{:#?}", expr);
                            Err(SpanError::new(SpanErrorKind::NotSynthCall, fn_item.span)
                                .into())
                        }
                    }
                } else {
                    Err(SpanError::new(SpanErrorKind::NotSynthCall, expr.span).into())
                }
            }
            ExprKind::Closure(closure) => {
                let body = self.tcx.hir().body(closure.body);
                let inputs = closure.fn_decl.inputs.iter().zip(body.params.iter());

                let mut item_ids = vec![];
                self.evaluate_inputs(inputs, ctx, true, &mut |item_id| {
                    item_ids.push(item_id);
                })?;

                let mut dummy_inputs = vec![];
                self.group_list
                    .deep_iter::<Error, _>(&item_ids, &mut |_, node_id| {
                        if self.net_list[node_id].is_dummy_input() {
                            dummy_inputs.push(node_id);
                        }

                        Ok(())
                    })?;

                let closure = self.evaluate_expr(body.value, ctx)?;

                self.net_list.add_dummy_inputs(closure, dummy_inputs);

                Ok(closure)
            }
            ExprKind::DropTemps(inner) => self.evaluate_expr(inner, ctx),
            ExprKind::Field(expr, ident) => {
                let group_id = self.evaluate_expr(expr, ctx)?;
                let group_id = group_id.group_id();

                let res = self.group_list[group_id]
                    .by_field(ident.as_str())
                    .ok_or_else(|| {
                        SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into()
                    });
                if res.is_err() {
                    println!("field: {}", ident);
                }
                res
            }
            ExprKind::If(cond, if_block, else_block) => {
                let prim_ty =
                    self.find_sig_ty(ty, ctx.generic_args, expr.span)?.prim_ty();

                let else_block = else_block.ok_or_else(|| {
                    SpanError::new(SpanErrorKind::ExpectedIfElseExpr, expr.span)
                })?;

                let cond = self.evaluate_expr(cond, ctx)?.node_id();
                let if_block = self.evaluate_expr(if_block, ctx)?.node_id();
                let else_block = self.evaluate_expr(else_block, ctx)?.node_id();

                let cond = self.net_list.only_one_node_out_id(cond);
                let if_block = self.net_list.only_one_node_out_id(if_block);
                let else_block = self.net_list.only_one_node_out_id(else_block);

                Ok(self
                    .net_list
                    .add_node(
                        ctx.module_id,
                        Mux2Node::new(
                            prim_ty,
                            cond,
                            if_block,
                            else_block,
                            self.idents.for_module(ctx.module_id).tmp(),
                        ),
                    )
                    .into())
            }
            ExprKind::Index(
                expr,
                Expr {
                    kind:
                        ExprKind::Lit(Spanned {
                            node: LitKind::Int(ind, ..),
                            ..
                        }),
                    ..
                },
                span,
            ) => {
                let ArrayTy(_, sig_ty) = self
                    .find_sig_ty(ty, ctx.generic_args, expr.span)?
                    .opt_array_ty()
                    .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedArray, span))?;
                let width = sig_ty.width();

                let expr = self.evaluate_expr(expr, ctx)?;

                bit_vec_trans(self, expr, ctx, |generator, ctx, bit_vec| {
                    let ty = PrimTy::BitVec(width);
                    let start = ind * width;
                    Ok((
                        generator.net_list.add_node(
                            ctx.module_id,
                            Splitter::new(
                                bit_vec,
                                [(ty, generator.idents.for_module(ctx.module_id).tmp())],
                                Some(start),
                            ),
                        ),
                        ty.into(),
                    ))
                })
            }
            ExprKind::Lit(lit) => {
                let prim_ty = self.find_sig_ty(ty, ctx.generic_args, lit.span)?.prim_ty();
                let value = blackbox::evaluate_lit(prim_ty, lit)?;

                Ok(self
                    .net_list
                    .add_node(
                        ctx.module_id,
                        ConstNode::new(
                            prim_ty,
                            value,
                            self.idents.for_module(ctx.module_id).tmp(),
                        ),
                    )
                    .into())
            }
            ExprKind::MethodCall(_, rec, args, span) => {
                let fn_did = self
                    .tcx
                    .typeck(expr.hir_id.owner)
                    .type_dependent_def_id(expr.hir_id)
                    .unwrap();
                let generic_args = self.extract_generic_args_for_fn(fn_did, expr, ctx)?;

                match self.find_local_impl_id(fn_did, generic_args, span)? {
                    Some((impl_id, generic_args)) => self.evaluate_impl_fn_call(
                        impl_id,
                        generic_args,
                        Some(rec.into()),
                        args.iter().map(Into::into),
                        ctx,
                    ),
                    None => {
                        let blackbox = self.find_blackbox(fn_did, generic_args, span)?;
                        if let Blackbox::StdConversion { from } = blackbox {
                            if let Some(TraitImpls {
                                trait_id,
                                fn_did,
                                impls,
                            }) = self.find_local_trait_impls(TraitKind::From)
                            {
                                let generic_args = self
                                    .maybe_swap_generic_args_for_conversion(
                                        from,
                                        generic_args,
                                    );
                                if let Some((impl_id, generic_args)) = self
                                    .find_local_impl_id_in_impls(
                                        trait_id,
                                        impls.iter().copied(),
                                        fn_did,
                                        generic_args,
                                        span,
                                    )?
                                {
                                    return self.evaluate_impl_fn_call(
                                        impl_id,
                                        generic_args,
                                        Some(rec.into()),
                                        args.iter().map(Into::into),
                                        ctx,
                                    );
                                }
                            }
                        }
                        blackbox.evaluate_expr(self, expr, ctx)
                    }
                }
            }
            ExprKind::Path(QPath::Resolved(_, Path { res, segments, .. })) => match res {
                Res::Local(_) if segments.len() == 1 => {
                    let ident = segments[0].ident;
                    self.item_id_for_ident(ctx.module_id, ident)
                }
                Res::Def(DefKind::AssocFn, def_id) | Res::Def(DefKind::Fn, def_id) => {
                    self.evaluate_closure_fn_without_params(*def_id, expr, ctx)
                }
                Res::Def(DefKind::Const, def_id) => {
                    if def_id.is_local() {
                        let (_, generics, body_id) = self
                            .tcx
                            .hir()
                            .expect_item(def_id.expect_local())
                            .expect_const();

                        if !generics.params.is_empty() {
                            println!("{:#?}", expr);
                            return Err(SpanError::new(
                                SpanErrorKind::NotSynthExpr,
                                expr.span,
                            )
                            .into());
                        }

                        let body = self.tcx.hir().body(body_id);

                        self.evaluate_expr(body.value, ctx)
                    } else {
                        let blackbox =
                            self.find_blackbox(*def_id, ctx.generic_args, expr.span)?;
                        blackbox.evaluate_expr(self, expr, ctx)
                    }
                }
                _ => {
                    println!("{:#?}", expr);
                    Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into())
                }
            },
            ExprKind::Path(QPath::TypeRelative(_, _)) => {
                let ty = self.node_type(expr.hir_id, ctx.generic_args);
                if ty.is_fn() {
                    let fn_ty_did = utils::ty_def_id(ty).unwrap();
                    self.evaluate_closure_fn_without_params(fn_ty_did, expr, ctx)
                } else {
                    println!("{:#?}", expr);
                    Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into())
                }
            }
            ExprKind::Struct(_, fields, ..) => self.make_struct_group_from_exprs(
                expr,
                fields.iter().map(|field| field.expr),
                ctx,
            ),
            ExprKind::Tup(exprs) => self
                .make_tuple_group(exprs.iter(), |generator, expr| {
                    generator.evaluate_expr(expr, ctx)
                }),
            ExprKind::Unary(UnOp::Not, inner) => {
                let comb = self.evaluate_expr(inner, ctx)?.node_id();
                let prim_ty =
                    self.find_sig_ty(ty, ctx.generic_args, expr.span)?.prim_ty();
                let sym = self.idents.for_module(ctx.module_id).tmp();

                let comb = self.net_list.only_one_node_out_id(comb);

                Ok((if prim_ty.is_bool() {
                    self.net_list
                        .add_node(ctx.module_id, NotNode::new(prim_ty, comb, sym))
                } else {
                    self.net_list
                        .add_node(ctx.module_id, BitNotNode::new(prim_ty, comb, sym))
                })
                .into())
            }
            _ => {
                println!("{:#?}", expr);
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into())
            }
        };

        if res.is_ok() {
            scope.exit();
        }

        res
    }

    fn maybe_swap_generic_args_for_conversion(
        &self,
        from: bool,
        generic_args: GenericArgsRef<'tcx>,
    ) -> GenericArgsRef<'tcx> {
        match from {
            true => generic_args,
            // Swap generic args from Into::<Self, T> -> From::<Self = T, T = Self>
            false => self.tcx.mk_args(&[generic_args[1], generic_args[0]]),
        }
    }

    fn evaluate_closure_fn_without_params(
        &mut self,
        fn_id: DefId,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let arg = match self.tcx.hir().find_parent(expr.hir_id) {
            Some(HirNode::Expr(Expr {
                kind: ExprKind::MethodCall(_, _, args, _),
                ..
            })) => args.iter().find(|arg| arg.hir_id == expr.hir_id),
            Some(HirNode::Expr(Expr {
                kind: ExprKind::Call(_, args),
                ..
            })) => args.iter().find(|arg| arg.hir_id == expr.hir_id),
            _ => None,
        };

        if let Some(arg) = arg {
            let span = arg.span;
            let ty = self.node_type(arg.hir_id, ctx.generic_args);
            let generic_args = self.subst_with(utils::subst(ty), ctx.generic_args);

            let fn_sig = self.fn_sig(fn_id, Some(generic_args));
            let input_ty = self.find_sig_ty(fn_sig.inputs()[0], generic_args, span)?;
            let output_ty = self.find_sig_ty(fn_sig.output(), generic_args, span)?;

            println!(
                "evaluate closure fn without params: {:?}: {:?}",
                fn_sig, generic_args
            );

            if fn_id.is_local() {
                let (input, dummy_inputs) =
                    self.make_dummy_inputs_from_sig_ty(input_ty, ctx);
                let closure = match self.find_local_impl_id(fn_id, generic_args, span)? {
                    Some((impl_id, generic_args)) => self.evaluate_impl_fn_call(
                        impl_id,
                        generic_args,
                        Some(input.into()),
                        [],
                        ctx,
                    )?,
                    None => self.evaluate_fn_call(
                        fn_id.expect_local(),
                        generic_args,
                        [input.into()],
                        ctx,
                    )?,
                };

                self.net_list.add_dummy_inputs(closure, dummy_inputs);

                return Ok(closure);
            } else {
                let blackbox = self.find_blackbox(fn_id, ctx.generic_args, span)?;
                if let Blackbox::StdConversion { from } = blackbox {
                    let (input, dummy_inputs) =
                        self.make_dummy_inputs_from_sig_ty(input_ty, ctx);

                    let closure = match self.find_local_trait_impls(TraitKind::From) {
                        Some(TraitImpls {
                            trait_id,
                            fn_did,
                            impls,
                        }) => {
                            let generic_args = self
                                .maybe_swap_generic_args_for_conversion(
                                    from,
                                    generic_args,
                                );
                            match self.find_local_impl_id_in_impls(
                                trait_id,
                                impls.iter().copied(),
                                fn_did,
                                generic_args,
                                span,
                            )? {
                                Some((impl_id, generic_args)) => {
                                    Some(self.evaluate_impl_fn_call(
                                        impl_id,
                                        generic_args,
                                        Some(input.into()),
                                        [],
                                        ctx,
                                    )?)
                                }
                                None => None,
                            }
                        }
                        None => None,
                    };

                    let closure = match closure {
                        Some(closure) => closure,
                        None => StdConversion::convert(
                            input_ty, output_ty, self, input, expr.span,
                        )?,
                    };

                    self.net_list.add_dummy_inputs(closure, dummy_inputs);

                    return Ok(closure);
                }
            }
        }
        Err(SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into())
    }

    fn extract_generic_args_for_fn(
        &self,
        fn_id: DefId,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<GenericArgsRef<'tcx>, Error> {
        let arg_matcher = ArgMatcher::new(self.tcx);

        let fn_args = self.subst_with(
            self.tcx.typeck(expr.hir_id.owner).node_args(expr.hir_id),
            ctx.generic_args,
        );

        let fn_generic_args =
            utils::subst(self.tcx.type_of(fn_id).instantiate_identity());

        let res = arg_matcher
            .extract_params(fn_id, fn_args, fn_generic_args)
            .ok_or_else(|| {
                println!("'{:?}' and '{:?}' does not match", fn_args, fn_generic_args);
                SpanError::new(SpanErrorKind::NotSynthExpr, expr.span).into()
            });

        println!(
            "extract generic args: fn_args {:?} == fn_generic_args {:?} ? {:?}",
            fn_args, fn_generic_args, res
        );

        res
    }

    pub fn instantiate_module(
        &mut self,
        module_id: ModuleId,
        self_arg: Option<ExprOrItemId<'tcx>>,
        args: impl IntoIterator<Item = ExprOrItemId<'tcx>>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let mut inputs = Vec::new();

        if let Some(self_arg) = self_arg {
            let self_arg = self_arg.evaluate(self, ctx)?;
            self.add_module_input(self_arg, &mut inputs);
        }

        for arg in args {
            let arg = arg.evaluate(self, ctx)?;
            self.add_module_input(arg, &mut inputs);
        }

        let outputs = self.net_list[module_id]
            .outputs()
            .map(|node_out_id| {
                (
                    self.net_list[node_out_id].ty,
                    self.idents.for_module(ctx.module_id).tmp(),
                )
            })
            .collect::<Vec<_>>();

        let inst_sym = self
            .idents
            .for_module(ctx.module_id)
            .inst(self.net_list[module_id].name);

        Ok(self
            .net_list
            .add_node(
                ctx.module_id,
                ModInst::new(inst_sym, module_id, inputs, outputs),
            )
            .into())
    }

    fn add_module_input(&self, item_id: ItemId, inputs: &mut Vec<NodeOutId>) {
        let mut evaluate = |_, node_id: NodeId| {
            for out in self.net_list[node_id].outputs().items() {
                inputs.push(out.node_out_id(node_id));
            }

            Result::<(), Error>::Ok(())
        };

        self.group_list
            .deep_iter(&[item_id], &mut evaluate)
            .unwrap();
    }

    pub fn find_local_impl_id(
        &self,
        fn_ty_did: DefId,
        generic_args: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<Option<(LocalDefId, GenericArgsRef<'tcx>)>, Error> {
        if fn_ty_did.is_local() && self.tcx.impl_of_method(fn_ty_did).is_some() {
            return Ok(Some((fn_ty_did.expect_local(), generic_args)));
        }

        if let Some(trait_id) = self.tcx.trait_of_item(fn_ty_did) {
            let impls = match self.tcx.all_local_trait_impls(()).get(&trait_id) {
                Some(impls) => impls,
                None => {
                    return Ok(None);
                }
            };

            return self.find_local_impl_id_in_impls(
                trait_id,
                impls.iter().copied(),
                fn_ty_did,
                generic_args,
                span,
            );
        }

        Ok(None)
    }

    fn find_local_impl_id_in_impls(
        &self,
        trait_id: DefId,
        impls: impl Iterator<Item = LocalDefId>,
        fn_ty_did: DefId,
        generic_args: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<Option<(LocalDefId, GenericArgsRef<'tcx>)>, Error> {
        // Maybe there is another way to find an impl_id for the specified method (fn_ty_did).
        // The current algorigthm finds impl_id by comparing fn signatures.
        let mut found_impl_id = None;
        let mut arg_matcher = ArgMatcher::new(self.tcx);

        for imp in impls {
            arg_matcher.clear();

            let trait_ref = self
                .tcx
                .impl_trait_ref(imp)
                .expect("expected trait ref")
                .skip_binder();
            let trait_generics = self
                .tcx
                .mk_args_from_iter(generic_args.iter().take(trait_ref.args.len()));

            let is_trait_match =
                arg_matcher.is_args_match(trait_generics, trait_ref.args);
            println!(
                "check trait parameters: trait_generics {:?} == trait_ref.args {:?} ? {}",
                trait_generics, trait_ref.args, is_trait_match
            );
            if !is_trait_match {
                continue;
            }

            if let Some(impl_item_id) = self
                .tcx
                .impl_item_implementor_ids(imp.to_def_id())
                .get(&fn_ty_did)
            {
                let fn_sig = self.fn_sig(fn_ty_did, Some(generic_args));

                let impl_fn_sig = self.fn_sig(*impl_item_id, None);

                let is_match = arg_matcher.is_tys_match(
                    fn_sig.inputs_and_output,
                    impl_fn_sig.inputs_and_output,
                );
                println!(
                    "check fn signatures: fn_sig {:?} == impl_fn_sig {:?} ? {}",
                    fn_sig.inputs_and_output, impl_fn_sig.inputs_and_output, is_match
                );

                if is_match {
                    if found_impl_id.is_some() {
                        let trait_name = self.tcx.def_path_str(trait_id);
                        return Err(SpanError::new(
                            SpanErrorKind::MultipleTraitImpls(trait_name),
                            span,
                        )
                        .into());
                    }

                    let impl_generic_args =
                        GenericArgs::for_item(self.tcx, *impl_item_id, |param, _| {
                            match arg_matcher.arg(param.index) {
                                Some(arg) => arg,
                                None => {
                                    panic!(
                                        "cannot find generic arg for param {:?}",
                                        param
                                    );
                                }
                            }
                        });

                    println!("impl generic args: {:?}", impl_generic_args);

                    if self.tcx.subst_and_check_impossible_predicates((
                        *impl_item_id,
                        impl_generic_args,
                    )) {
                        println!("predicates are impossible");
                        continue;
                    }

                    found_impl_id =
                        Some((impl_item_id.expect_local(), impl_generic_args));
                }
            }
        }

        Ok(found_impl_id)
    }

    pub fn fn_sig(
        &self,
        def_id: DefId,
        subst: Option<GenericArgsRef<'tcx>>,
    ) -> FnSig<'tcx> {
        let fn_sig = self.tcx.fn_sig(def_id);

        (match subst {
            Some(subst) => fn_sig.instantiate(self.tcx, subst),
            None => fn_sig.instantiate_identity(),
        })
        .skip_binder()
    }

    pub fn evaluate_fn_call(
        &mut self,
        fn_id: LocalDefId,
        generic_args: GenericArgsRef<'tcx>,
        args: impl IntoIterator<Item = ExprOrItemId<'tcx>>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let mono_item = MonoItem::new(fn_id, generic_args);

        #[allow(clippy::map_entry)]
        if !self.evaluated_modules.contains_key(&mono_item) {
            println!(
                "evaluate fn call: fn_id = {:?}, generic_args = {:?}",
                fn_id, generic_args
            );
            let item = self.tcx.hir().expect_item(fn_id);
            let module_id = self.evaluate_fn_item(item, generic_args, false)?.unwrap();

            self.evaluated_modules.insert(mono_item, module_id);
        }

        let module_id = self.evaluated_modules.get(&mono_item).unwrap();

        self.instantiate_module(*module_id, None, args, ctx)
    }

    pub fn evaluate_impl_fn_call(
        &mut self,
        impl_id: LocalDefId,
        generic_args: GenericArgsRef<'tcx>,
        self_arg: Option<ExprOrItemId<'tcx>>,
        args: impl IntoIterator<Item = ExprOrItemId<'tcx>>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let mono_item = MonoItem::new(impl_id, generic_args);

        #[allow(clippy::map_entry)]
        if !self.evaluated_modules.contains_key(&mono_item) {
            println!(
                "evaluate impl fn call: impl_id = {:?}, generic_args = {:?}",
                impl_id, generic_args
            );
            let impl_item = self.tcx.hir().expect_impl_item(impl_id);
            let module_id = self.evaluate_impl_item(impl_item, generic_args)?.unwrap();

            self.evaluated_modules.insert(mono_item, module_id);
        }

        let module_id = self.evaluated_modules.get(&mono_item).unwrap();

        self.instantiate_module(*module_id, self_arg, args, ctx)
    }
}
