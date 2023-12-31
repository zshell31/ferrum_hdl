use fhdl_netlist::{
    group::ItemId,
    node::Splitter,
    sig_ty::{NodeTy, SignalTy},
};
use rustc_hir::Expr;

use super::{bitvec, EvalExpr};
use crate::{
    error::Error, eval_context::EvalContext, generator::Generator, scopes::SymIdent,
    utils,
};

pub struct BitPackPack;

impl<'tcx> EvalExpr<'tcx> for BitPackPack {
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

impl<'tcx> EvalExpr<'tcx> for BitPackRepack {
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
        let sig_ty = generator.find_sig_ty(ty, ctx, expr.span)?;

        Ok(generator.from_bitvec(ctx.module_id, to, sig_ty))
    }
}

pub struct BitPackMsb;

impl<'tcx> EvalExpr<'tcx> for BitPackMsb {
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
                generator.netlist.add(
                    ctx.module_id,
                    Splitter::new(bit_vec, [(ty, SymIdent::Msb)], None, true),
                ),
                SignalTy::new(ty.into()),
            ))
        })
    }
}
