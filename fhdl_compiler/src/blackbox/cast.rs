use std::iter;

use ferrum_hdl::{
    bit::Bit,
    cast::CastFrom,
    unsigned::{u, Unsigned},
};
use fhdl_netlist::{
    group::ItemId,
    net_list::ModuleId,
    sig_ty::{ArrayTy, NodeTy, SignalTy, SignalTyKind},
};
use rustc_hir::{Expr, HirId};
use rustc_middle::ty::{GenericArgsRef, TyKind};
use rustc_span::Span;

use super::EvalExpr;
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    generator::{expr::ExprOrItemId, Generator, TraitKind},
    utils,
};

#[allow(dead_code)]
pub struct Conversion {
    pub from: bool,
}

impl Conversion {
    #[allow(dead_code)]
    pub fn new(from: bool) -> Self {
        Self { from }
    }
}

impl Conversion {
    pub fn convert_as_prim_ty(
        module_id: ModuleId,
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
                Ok(Self::to_unsigned(module_id, from, to_ty, generator))
            }
            _ => {
                println!("from {:?} => to {:?}", from_ty, to_ty);

                Err(SpanError::new(SpanErrorKind::UnsupportedConversion, span).into())
            }
        }
    }

    pub fn convert<'tcx>(
        &self,
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
            ) if generator.is_unsigned_short(&to_ty)
                && from_ty.width() == to_ty.width() =>
            {
                assert_convert::<Unsigned<1>, u<1>>();
                generator
                    .make_struct_group(struct_ty, iter::once(from), |_, item| Ok(item))
            }
            (
                SignalTyKind::Struct(_),
                SignalTyKind::Node(to_ty @ NodeTy::Unsigned(_)),
            ) if generator.is_unsigned_short(&from_ty)
                && from_ty.width() == to_ty.width() =>
            {
                assert_convert::<u<1>, Unsigned<1>>();

                let from = from.group().item_ids()[0];
                Ok(Self::to_unsigned(ctx.module_id, from, to_ty, generator))
            }
            (SignalTyKind::Array(from_ty), SignalTyKind::Array(to_ty))
                if from_ty.count() == to_ty.count() =>
            {
                self.cast_array(generator, expr, from, to_ty, ctx)
            }
            _ => {
                Self::convert_as_prim_ty(ctx.module_id, from, to_ty, generator, expr.span)
            }
        }
    }

    #[allow(dead_code)]
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
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        from: ItemId,
        to_ty: ArrayTy,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let fn_item = utils::expected_call(expr)?.fn_item;
        let generic_args = generator.extract_generic_args(fn_item, ctx);
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
                let to_item_ty = to_ty.item_ty();
                generator.make_array_group(to_ty, from, |generator, from| {
                    self.convert(generator, expr, ctx, *from, to_item_ty)
                })
            }
        }
    }

    fn try_local_cast<'tcx>(
        &self,
        generator: &mut Generator<'tcx>,
        fn_item: HirId,
        from: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<Option<ItemId>, Error> {
        let generic_args = generator.extract_generic_args(fn_item, ctx);

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

    fn to_unsigned(
        module_id: ModuleId,
        from: ItemId,
        to_ty: NodeTy,
        generator: &mut Generator<'_>,
    ) -> ItemId {
        generator
            .trunc_or_extend(module_id, from.node_out_id(), to_ty)
            .into()
    }
}

fn assert_convert<F, T: CastFrom<F>>() {}

impl<'tcx> EvalExpr<'tcx> for Conversion {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as fn_item with from);

        match self.try_local_cast(generator, fn_item, from, ctx, expr.span)? {
            Some(item_id) => Ok(item_id),
            None => {
                let to_ty = generator.find_sig_ty(
                    generator.node_type(expr.hir_id, ctx),
                    ctx,
                    expr.span,
                )?;

                let from = generator.eval_expr(from, ctx)?;

                self.convert(generator, expr, ctx, from, to_ty)
            }
        }
    }
}
