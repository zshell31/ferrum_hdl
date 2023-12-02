use fhdl_netlist::{
    group::ItemId,
    net_list::{NodeId, NodeOutId},
    node::{IsNode, LoopEnd, LoopStart, NodeKind, Splitter},
    params::Outputs,
    sig_ty::{PrimTy, SignalTy},
    symbol::Symbol,
};
use rustc_hir::Expr;

use super::EvaluateExpr;
use crate::{
    error::Error,
    generator::{EvalContext, Generator},
    utils,
};

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
    let trans = generator.net_list[trans]
        .kind
        .outputs()
        .only_one()
        .node_out_id(trans);

    let from = generator.from_bitvec(ctx.module_id, trans, sig_ty);

    Ok(from)
}

pub struct LoopArgs {
    pub input: NodeOutId,
    pub output: Option<Symbol>,
    pub loop_var: Symbol,
}

pub fn bit_vec_trans_in_loop<'tcx>(
    generator: &mut Generator<'tcx>,
    source: ItemId,
    ctx: &EvalContext<'tcx>,
    count: u128,
    trans: impl FnOnce(
        &mut Generator<'tcx>,
        &EvalContext<'tcx>,
        LoopArgs,
    ) -> Result<SignalTy, Error>,
) -> Result<ItemId, Error> {
    bit_vec_trans(generator, source, ctx, |generator, ctx, bit_vec| {
        let loop_var = Symbol::new("i");
        let output = None;

        let loop_id = generator
            .net_list
            .add(ctx.module_id, LoopStart::new(loop_var, count, None));

        let sig_ty = trans(generator, ctx, LoopArgs {
            input: bit_vec,
            output,
            loop_var,
        })?;
        let width = sig_ty.width();

        generator.net_list.add(ctx.module_id, LoopEnd {});

        if let NodeKind::LoopStart(node) = &mut generator.net_list[loop_id].kind {
            node.set_out(Some((PrimTy::BitVec(count * width), output)));
        }

        Ok((loop_id, SignalTy::mk_array(None, count, sig_ty)))
    })
}

pub struct BitVecShrink;

impl<'tcx> EvaluateExpr<'tcx> for BitVecShrink {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.evaluate_expr(rec, ctx)?;

        let ty = generator.node_type(expr.hir_id, ctx);
        let width = generator
            .find_sig_ty(ty, ctx.generic_args, expr.span)?
            .width();

        let rec = generator.maybe_to_bitvec(ctx.module_id, rec);

        Ok(generator
            .net_list
            .add(
                ctx.module_id,
                Splitter::new(rec, [(PrimTy::BitVec(width), None)], None, false),
            )
            .into())
    }
}

pub struct BitVecSlice;

impl<'tcx> EvaluateExpr<'tcx> for BitVecSlice {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.evaluate_expr(rec, ctx)?.node_id();
        let rec = generator.net_list[rec]
            .kind
            .outputs()
            .only_one()
            .node_out_id(rec);

        let generics = generator.method_call_generics(expr, ctx)?;

        let start = generics.as_const(1).unwrap();
        let width = generics.as_const(2).unwrap();

        Ok(generator
            .net_list
            .add(
                ctx.module_id,
                Splitter::new(rec, [(PrimTy::BitVec(width), None)], Some(start), false),
            )
            .into())
    }
}

pub struct BitVecUnpack;

impl<'tcx> EvaluateExpr<'tcx> for BitVecUnpack {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.evaluate_expr(rec, ctx)?.node_id();
        let rec = generator.net_list[rec]
            .kind
            .outputs()
            .only_one()
            .node_out_id(rec);

        let ty = generator.node_type(expr.hir_id, ctx);
        let sig_ty = generator.find_sig_ty(ty, ctx.generic_args, expr.span)?;

        Ok(generator.from_bitvec(ctx.module_id, rec, sig_ty))
    }
}
