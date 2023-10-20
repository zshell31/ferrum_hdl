use std::iter;

use ferrum_hdl::{
    bit::Bit,
    unsigned::{u, Unsigned},
};
use fhdl_netlist::{
    group::ItemId,
    node::{IsNode, Splitter, ZeroExtend},
    params::Outputs,
    sig_ty::{NodeTy, SignalTy, SignalTyKind},
};
use rustc_hir::Expr;
use rustc_span::Span;

use super::EvaluateExpr;
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    generator::{EvalContext, Generator},
    utils,
};

pub struct Cast;

impl<'tcx> EvaluateExpr<'tcx> for Cast {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        StdConversion { from: false }.evaluate_expr(generator, expr, ctx)
    }
}

#[allow(dead_code)]
pub struct StdConversion {
    pub from: bool,
}

impl StdConversion {
    fn bool_to_bit(from: ItemId, generator: &mut Generator<'_>) -> ItemId {
        let node_out = generator.net_list[from.node_id()]
            .kind
            .outputs_mut()
            .only_one_mut();
        node_out.out.ty = NodeTy::Bit;
        from
    }

    fn bit_to_bool(from: ItemId, generator: &mut Generator<'_>) -> ItemId {
        let node_out = generator.net_list[from.node_id()]
            .kind
            .outputs_mut()
            .only_one_mut();
        node_out.out.ty = NodeTy::Bool;
        from
    }

    #[inline(always)]
    fn to_u8(from: ItemId, generator: &mut Generator<'_>) -> ItemId {
        Self::to_unsigned_(from, NodeTy::U8, generator)
    }

    #[inline(always)]
    fn to_u16(from: ItemId, generator: &mut Generator<'_>) -> ItemId {
        Self::to_unsigned_(from, NodeTy::U16, generator)
    }

    #[inline(always)]
    fn to_u32(from: ItemId, generator: &mut Generator<'_>) -> ItemId {
        Self::to_unsigned_(from, NodeTy::U32, generator)
    }

    #[inline(always)]
    fn to_u64(from: ItemId, generator: &mut Generator<'_>) -> ItemId {
        Self::to_unsigned_(from, NodeTy::U64, generator)
    }

    #[inline(always)]
    fn to_u128(from: ItemId, generator: &mut Generator<'_>) -> ItemId {
        Self::to_unsigned_(from, NodeTy::U128, generator)
    }

    #[inline(always)]
    fn to_unsigned(from: ItemId, width: u128, generator: &mut Generator<'_>) -> ItemId {
        Self::to_unsigned_(from, NodeTy::Unsigned(width), generator)
    }

    fn to_unsigned_(from: ItemId, ty: NodeTy, generator: &mut Generator<'_>) -> ItemId {
        let node_id = from.node_id();
        let module_id = node_id.module_id();
        let node_out_id = generator.net_list[node_id]
            .kind
            .outputs()
            .only_one()
            .node_out_id(node_id);

        if generator.item_ty(from).width() >= ty.width() {
            generator
                .net_list
                .add(
                    module_id,
                    Splitter::new(node_out_id, [(ty, None)], None, false),
                )
                .into()
        } else {
            generator
                .net_list
                .add(module_id, ZeroExtend::new(ty, node_out_id, None))
                .into()
        }
    }

    pub fn convert(
        from_ty: SignalTy,
        to_ty: SignalTy,
        from: ItemId,
        generator: &mut Generator<'_>,
        span: Span,
    ) -> Result<ItemId, Error> {
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
            (SignalTyKind::Node(NodeTy::U8), SignalTyKind::Node(NodeTy::Unsigned(n))) => {
                assert_convert::<u8, Unsigned<1>>();
                Ok(Self::to_unsigned(from, n, generator))
            }
            (SignalTyKind::Node(NodeTy::Unsigned(_)), SignalTyKind::Node(NodeTy::U8)) => {
                assert_convert::<Unsigned<1>, u8>();
                Ok(Self::to_u8(from, generator))
            }
            (
                SignalTyKind::Node(NodeTy::U16),
                SignalTyKind::Node(NodeTy::Unsigned(n)),
            ) => {
                assert_convert::<u16, Unsigned<1>>();
                Ok(Self::to_unsigned(from, n, generator))
            }
            (
                SignalTyKind::Node(NodeTy::Unsigned(_)),
                SignalTyKind::Node(NodeTy::U16),
            ) => {
                assert_convert::<Unsigned<1>, u16>();
                Ok(Self::to_u16(from, generator))
            }
            (
                SignalTyKind::Node(NodeTy::U32),
                SignalTyKind::Node(NodeTy::Unsigned(n)),
            ) => {
                assert_convert::<u32, Unsigned<1>>();
                Ok(Self::to_unsigned(from, n, generator))
            }
            (
                SignalTyKind::Node(NodeTy::Unsigned(_)),
                SignalTyKind::Node(NodeTy::U32),
            ) => {
                assert_convert::<Unsigned<1>, u32>();
                Ok(Self::to_u32(from, generator))
            }
            (
                SignalTyKind::Node(NodeTy::U64),
                SignalTyKind::Node(NodeTy::Unsigned(n)),
            ) => {
                assert_convert::<u64, Unsigned<1>>();
                Ok(Self::to_unsigned(from, n, generator))
            }
            (
                SignalTyKind::Node(NodeTy::Unsigned(_)),
                SignalTyKind::Node(NodeTy::U64),
            ) => {
                assert_convert::<Unsigned<1>, u64>();
                Ok(Self::to_u64(from, generator))
            }
            (
                SignalTyKind::Node(NodeTy::U128),
                SignalTyKind::Node(NodeTy::Unsigned(n)),
            ) => {
                assert_convert::<u128, Unsigned<1>>();
                Ok(Self::to_unsigned(from, n, generator))
            }
            (
                SignalTyKind::Node(NodeTy::Unsigned(_)),
                SignalTyKind::Node(NodeTy::U128),
            ) => {
                assert_convert::<Unsigned<1>, u128>();
                Ok(Self::to_u128(from, generator))
            }
            (
                SignalTyKind::Node(NodeTy::Unsigned(_)),
                SignalTyKind::Struct(struct_ty),
            ) if to_ty.is_unsigned_short() && from_ty.width() == to_ty.width() => {
                assert_convert::<Unsigned<1>, u<1>>();
                generator
                    .make_struct_group(struct_ty, iter::once(from), |_, item| Ok(item))
            }
            (SignalTyKind::Struct(_), SignalTyKind::Node(NodeTy::Unsigned(n)))
                if from_ty.is_unsigned_short() && from_ty.width() == to_ty.width() =>
            {
                assert_convert::<u<1>, Unsigned<1>>();

                let from = from.group().item_ids()[0];
                Ok(Self::to_unsigned(from, n, generator))
            }
            (SignalTyKind::Array(from_ty), SignalTyKind::Array(to_ty))
                if from_ty.count() == to_ty.count() =>
            {
                let ty = to_ty;
                let from_ty = *from_ty.item_ty();
                let to_ty = *to_ty.item_ty();
                let from = from.group();

                generator.make_array_group(ty, from.item_ids(), |generator, item_id| {
                    Self::convert(from_ty, to_ty, *item_id, generator, span)
                })
            }
            _ => {
                println!("from {:?} => to {:?}", from_ty, to_ty);

                Err(SpanError::new(SpanErrorKind::UnsupportedConversion, span).into())
            }
        }
    }
}

fn assert_convert<F, T: From<F>>() {}

impl<'tcx> EvaluateExpr<'tcx> for StdConversion {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as from);

        let span = expr.span;

        let from_ty = generator.find_sig_ty(
            generator.node_type(from.hir_id, ctx),
            ctx.generic_args,
            span,
        )?;

        let to_ty = generator.find_sig_ty(
            generator.node_type(expr.hir_id, ctx),
            ctx.generic_args,
            span,
        )?;

        let from = generator.evaluate_expr(from, ctx)?;

        Self::convert(from_ty, to_ty, from, generator, expr.span)
    }
}
