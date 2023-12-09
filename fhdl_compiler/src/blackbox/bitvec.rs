use fhdl_netlist::{
    group::ItemId,
    net_list::{NodeId, NodeOutId},
    node::Splitter,
    sig_ty::{NodeTy, SignalTy},
    symbol::Symbol,
};
use rustc_hir::Expr;

use super::EvalExpr;
use crate::{error::Error, eval_context::EvalContext, generator::Generator, utils};

pub fn bit_vec_trans<'tcx>(
    generator: &mut Generator<'tcx>,
    source: ItemId,
    ctx: &EvalContext<'tcx>,
    trans: impl FnOnce(
        &mut Generator<'tcx>,
        &EvalContext<'tcx>,
        NodeOutId,
    ) -> Result<(NodeId, SignalTy), Error>,
) -> Result<ItemId, Error> {
    let bit_vec = generator.to_bitvec(ctx.module_id, source);

    let (trans, sig_ty) = trans(generator, ctx, bit_vec)?;
    let trans = generator.netlist[trans].only_one_out().node_out_id();

    let from = generator.from_bitvec(ctx.module_id, trans, sig_ty);

    Ok(from)
}

pub struct BitVecShrink;

impl<'tcx> EvalExpr<'tcx> for BitVecShrink {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.eval_expr(rec, ctx)?;

        let ty = generator.node_type(expr.hir_id, ctx);
        let width = generator.find_sig_ty(ty, ctx, expr.span)?.width();

        let rec = generator.to_bitvec(ctx.module_id, rec);

        Ok(generator
            .netlist
            .add_and_get_out(
                ctx.module_id,
                Splitter::new(rec, [(NodeTy::BitVec(width), None)], None, false),
            )
            .into())
    }
}

pub struct BitVecSlice;

impl<'tcx> EvalExpr<'tcx> for BitVecSlice {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.eval_expr(rec, ctx)?.node_out_id();

        let generics = generator.method_call_generics(expr, ctx)?;

        let start = generics.as_const_opt(1).unwrap();
        let width = generics.as_const_opt(2).unwrap();

        let sym = generator.netlist[rec]
            .sym
            .map(|sym| Symbol::new_from_args(format_args!("{}_slice", sym)));

        Ok(generator
            .netlist
            .add_and_get_out(
                ctx.module_id,
                Splitter::new(rec, [(NodeTy::BitVec(width), sym)], Some(start), false),
            )
            .into())
    }
}

pub struct BitVecUnpack;

impl<'tcx> EvalExpr<'tcx> for BitVecUnpack {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.eval_expr(rec, ctx)?.node_out_id();

        let ty = generator.node_type(expr.hir_id, ctx);
        let sig_ty = generator.find_sig_ty(ty, ctx, expr.span)?;

        Ok(generator.from_bitvec(ctx.module_id, rec, sig_ty))
    }
}
