use std::iter;

use ferrum_hdl::{
    bit::Bit,
    cast::CastFrom,
    unsigned::{u, Unsigned},
};
use fhdl_netlist::{
    group::ItemId,
    node::{NodeOutput, Splitter, ZeroExtend},
    sig_ty::{ArrayTy, NodeTy, SignalTy, SignalTyKind},
};
use rustc_hir::{Expr, HirId};
use rustc_middle::ty::{GenericArgsRef, TyKind};
use rustc_span::Span;

use super::{Blackbox, EvalExpr};
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    generator::{expr::ExprOrItemId, metadata::TemplateNodeKind, Generator, TraitKind},
    utils,
};

#[allow(dead_code)]
pub struct Conversion {
    pub from: bool,
}

impl Conversion {
    pub fn new(from: bool) -> Self {
        Self { from }
    }
}

impl Conversion {
    pub fn convert_as_prim_ty(
        from: ItemId,
        to_ty: SignalTy,
        generator: &mut Generator<'_>,
        span: Span,
    ) -> Result<ItemId, Error> {
        let from_ty = generator.item_ty(from);
        if from_ty.kind == to_ty.kind {
            return Ok(from);
        }

        match (from_ty.kind, to_ty.kind) {
            (SignalTyKind::Node(NodeTy::Bool), SignalTyKind::Node(NodeTy::Bit)) => {
                assert_convert::<bool, Bit>();
                Ok(Self::bool_to_bit(from, generator))
            }
            (SignalTyKind::Node(NodeTy::Bit), SignalTyKind::Node(NodeTy::Bool)) => {
                assert_convert::<Bit, bool>();
                Ok(Self::bit_to_bool(from, generator))
            }
            (SignalTyKind::Node(from_ty), SignalTyKind::Node(to_ty))
                if from_ty.is_unsigned() && to_ty.is_unsigned() =>
            {
                Ok(Self::to_unsigned(from, to_ty, generator))
            }
            _ => {
                println!("from {:?} => to {:?}", from_ty, to_ty);

                Err(SpanError::new(SpanErrorKind::UnsupportedConversion, span).into())
            }
        }
    }

    pub fn convert<'tcx>(
        &self,
        blackbox: &Blackbox,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        from: ItemId,
        to_ty: SignalTy,
    ) -> Result<ItemId, Error> {
        let from_ty = generator.item_ty(from);
        if from_ty.kind == to_ty.kind {
            return Ok(from);
        }

        match (from_ty.kind, to_ty.kind) {
            (
                SignalTyKind::Node(NodeTy::Unsigned(_)),
                SignalTyKind::Struct(struct_ty),
            ) if to_ty.is_unsigned_short() && from_ty.width() == to_ty.width() => {
                assert_convert::<Unsigned<1>, u<1>>();
                generator
                    .make_struct_group(struct_ty, iter::once(from), |_, item| Ok(item))
            }
            (
                SignalTyKind::Struct(_),
                SignalTyKind::Node(to_ty @ NodeTy::Unsigned(_)),
            ) if from_ty.is_unsigned_short() && from_ty.width() == to_ty.width() => {
                assert_convert::<u<1>, Unsigned<1>>();

                let from = from.group().item_ids()[0];
                Ok(Self::to_unsigned(from, to_ty, generator))
            }
            (SignalTyKind::Array(from_ty), SignalTyKind::Array(to_ty))
                if from_ty.count() == to_ty.count() =>
            {
                self.cast_array(blackbox, generator, expr, from, to_ty, ctx)
            }
            _ => Self::convert_as_prim_ty(from, to_ty, generator, expr.span),
        }
    }

    pub fn eval_cast_as_closure<'tcx>(
        &self,
        generator: &mut Generator<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        generic_args: GenericArgsRef<'tcx>,
        from: ItemId,
        span: Span,
    ) -> Result<Option<ItemId>, Error> {
        self.try_local_cast_(generator, ctx, generic_args, from.into(), span)
    }

    fn cast_array<'tcx>(
        &self,
        blackbox: &Blackbox,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        from: ItemId,
        to_ty: ArrayTy,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let fn_item = utils::expected_call(expr)?.fn_item;
        let fn_did = blackbox.fn_did;
        let generic_args = generator.extract_generic_args(fn_did, fn_item, ctx)?;
        let generic_args =
            generator
                .tcx
                .mk_args_from_iter(generic_args.iter().map(|generic_arg| {
                    match generic_arg.as_type() {
                        Some(ty) if ty.is_array() => match ty.kind() {
                            TyKind::Array(ty, _) => (*ty).into(),
                            _ => unreachable!(),
                        },
                        _ => generic_arg,
                    }
                }));

        // TODO: use loop
        let from = from.group().item_ids();
        match generator
            .find_trait_method(TraitKind::From)
            .and_then(|fn_from_did| {
                let generic_args = generator
                    .maybe_swap_generic_args_for_conversion(self.from, generic_args);

                generator.find_local_impl_id(fn_from_did, generic_args)
            }) {
            Some((impl_id, generic_args)) => {
                generator.make_array_group(to_ty, from, |generator, from| {
                    generator.eval_impl_fn_call(
                        impl_id,
                        generic_args,
                        Some((*from).into()),
                        iter::empty(),
                        ctx,
                        expr.span,
                    )
                })
            }
            None => {
                let to_item_ty = *to_ty.item_ty();
                generator.make_array_group(to_ty, from, |generator, from| {
                    self.convert(blackbox, generator, expr, ctx, *from, to_item_ty)
                })
            }
        }
    }

    fn try_local_cast<'tcx>(
        &self,
        blackbox: &Blackbox,
        generator: &mut Generator<'tcx>,
        fn_item: HirId,
        from: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<Option<ItemId>, Error> {
        let fn_did = blackbox.fn_did;
        let generic_args = generator.extract_generic_args(fn_did, fn_item, ctx)?;

        self.try_local_cast_(generator, ctx, generic_args, from.into(), span)
    }

    fn try_local_cast_<'tcx>(
        &self,
        generator: &mut Generator<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        generic_args: GenericArgsRef<'tcx>,
        from: ExprOrItemId<'tcx>,
        span: Span,
    ) -> Result<Option<ItemId>, Error> {
        generator
            .find_trait_method(TraitKind::From)
            .and_then(|fn_from_did| {
                let generic_args = generator
                    .maybe_swap_generic_args_for_conversion(self.from, generic_args);

                generator.find_local_impl_id(fn_from_did, generic_args).map(
                    |(impl_id, generic_args)| {
                        generator.eval_impl_fn_call(
                            impl_id,
                            generic_args,
                            Some(from),
                            iter::empty(),
                            ctx,
                            span,
                        )
                    },
                )
            })
            .transpose()
    }

    fn bool_to_bit(from: ItemId, generator: &mut Generator<'_>) -> ItemId {
        generator.netlist[from.node_out_id()].ty = NodeTy::Bit;
        from
    }

    fn bit_to_bool(from: ItemId, generator: &mut Generator<'_>) -> ItemId {
        generator.netlist[from.node_out_id()].ty = NodeTy::Bool;
        from
    }

    pub fn to_unsigned(
        from: ItemId,
        to_ty: NodeTy,
        generator: &mut Generator<'_>,
    ) -> ItemId {
        let node_out_id = from.node_out_id();
        let module_id = node_out_id.node_id().module_id();
        let from_ty = generator.item_ty(from);

        if let (Some(from_width), Some(to_width)) =
            (from_ty.width().opt_value(), to_ty.width().opt_value())
        {
            if from_width >= to_width {
                generator
                    .netlist
                    .add_and_get_out(
                        module_id,
                        Splitter::new(node_out_id, [(to_ty, None)], None, false),
                    )
                    .into()
            } else {
                generator
                    .netlist
                    .add_and_get_out(module_id, ZeroExtend::new(to_ty, node_out_id, None))
                    .into()
            }
        } else {
            let node_id = generator.add_temp_node(
                module_id,
                TemplateNodeKind::CastToUnsigned {
                    from: node_out_id,
                    to_ty,
                },
                [node_out_id],
                [NodeOutput::wire(to_ty, None)],
            );

            generator.netlist[node_id]
                .only_one_out()
                .node_out_id()
                .into()
        }
    }
}

fn assert_convert<F, T: CastFrom<F>>() {}

impl<'tcx> EvalExpr<'tcx> for Conversion {
    fn eval_expr(
        &self,
        blackbox: &Blackbox,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as fn_item with from);

        match self.try_local_cast(blackbox, generator, fn_item, from, ctx, expr.span)? {
            Some(item_id) => Ok(item_id),
            None => {
                let to_ty = generator.find_sig_ty(
                    generator.node_type(expr.hir_id, ctx),
                    ctx,
                    expr.span,
                )?;

                let from = generator.eval_expr(from, ctx)?;

                self.convert(blackbox, generator, expr, ctx, from, to_ty)
            }
        }
    }
}
