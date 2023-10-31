use fhdl_netlist::{
    group::ItemId,
    node::Splitter,
    sig_ty::{NodeTy, SignalTy},
};
use rustc_hir::Expr;

use super::{bitvec, EvaluateExpr};
use crate::{
    error::Error, eval_context::EvalContext, generator::Generator, scopes::SymIdent,
    utils,
};

pub struct BitPackPack;

impl<'tcx> EvaluateExpr<'tcx> for BitPackPack {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);
        let rec = generator.eval_expr(rec, ctx)?;

        Ok(generator.to_bitvec(ctx.module_id, rec).into())
    }
}

pub struct BitPackRepack;

impl<'tcx> EvaluateExpr<'tcx> for BitPackRepack {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.eval_expr(rec, ctx)?;
        let to = generator.to_bitvec(ctx.module_id, rec);

        let ty = generator.node_type(expr.hir_id, ctx);
        let sig_ty = generator.find_sig_ty(ty, ctx.generic_args, expr.span)?;

        Ok(generator.from_bitvec(ctx.module_id, to, sig_ty))
    }
}

pub struct BitPackMsb;

impl<'tcx> EvaluateExpr<'tcx> for BitPackMsb {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        let rec = generator.eval_expr(rec, ctx)?;

        bitvec::bit_vec_trans(generator, rec, ctx, |generator, ctx, bit_vec| {
            let ty = NodeTy::Bit;

            Ok((
                generator.net_list.add(
                    ctx.module_id,
                    Splitter::new(bit_vec, [(ty, SymIdent::Msb)], None, true),
                ),
                SignalTy::new(None, ty.into()),
            ))
        })
    }
}
