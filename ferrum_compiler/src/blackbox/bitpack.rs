use ferrum_netlist::{group::ItemId, node::Splitter, sig_ty::PrimTy};
use rustc_hir::Expr;

use super::{bitvec, EvaluateExpr};
use crate::{
    error::Error,
    generator::{EvalContext, Generator},
    utils,
};

pub struct BitPackPack;

impl<'tcx> EvaluateExpr<'tcx> for BitPackPack {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;
        let rec = generator.evaluate_expr(rec, ctx)?;

        Ok(generator.to_bitvec(ctx.module_id, rec).node_id().into())
    }
}

pub struct BitPackRepack;

impl<'tcx> EvaluateExpr<'tcx> for BitPackRepack {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;
        let rec = generator.evaluate_expr(rec, ctx)?;
        let to = generator.to_bitvec(ctx.module_id, rec);

        let ty = generator.node_type(expr.hir_id, ctx);
        let sig_ty = generator.find_sig_ty(ty, ctx.generic_args, expr.span)?;

        Ok(generator.from_bitvec(ctx.module_id, to, sig_ty))
    }
}

pub struct BitPackMsb;

impl<'tcx> EvaluateExpr<'tcx> for BitPackMsb {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;
        let rec = generator.evaluate_expr(rec, ctx)?;

        bitvec::bit_vec_trans(generator, rec, ctx, |generator, ctx, bit_vec| {
            let ty = PrimTy::Bit;

            Ok((
                generator.net_list.add_node(
                    ctx.module_id,
                    Splitter::new(
                        bit_vec,
                        [(ty, generator.idents.for_module(ctx.module_id).tmp())],
                        None,
                        true,
                    ),
                ),
                ty.into(),
            ))
        })
    }
}
