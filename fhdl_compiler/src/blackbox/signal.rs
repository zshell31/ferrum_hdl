use fhdl_blackbox::Blackbox;
use fhdl_netlist::{
    group::ItemId,
    node::{BinOp, DFF},
};
use rustc_hir::Expr;
use rustc_span::Span;

use super::EvaluateExpr;
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    generator::Generator,
    scopes::SymIdent,
    utils,
};

pub struct SignalReg {
    pub has_en: bool,
}

impl SignalReg {
    fn make_err(&self, span: Span) -> Error {
        SpanError::new(
            SpanErrorKind::NotSynthBlackboxExpr(if !self.has_en {
                Blackbox::SignalReg
            } else {
                Blackbox::SignalRegEn
            }),
            span,
        )
        .into()
    }
}

impl<'tcx> EvaluateExpr<'tcx> for SignalReg {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let args = utils::expected_call(expr)?;

        let ty = generator.node_type(expr.hir_id, ctx);
        let value_ty =
            utils::subst_type(ty, 1).ok_or_else(|| self.make_err(expr.span))?;
        let sig_ty = generator.find_sig_ty(value_ty, ctx.generic_args, expr.span)?;

        let (clk, rst, en, rst_val, comb) = match self.has_en {
            true => (&args[0], &args[1], Some(&args[2]), &args[3], &args[4]),
            false => (&args[0], &args[1], None, &args[2], &args[3]),
        };

        let clk = generator.evaluate_expr(clk, ctx)?.node_out_id();
        let rst = generator
            .evaluate_expr(rst, ctx)
            .map(|item_id| item_id.node_out_id())?;
        let en = en
            .map(|en| {
                generator
                    .evaluate_expr(en, ctx)
                    .map(|item_id| item_id.node_out_id())
            })
            .transpose()?;
        let rst_val = generator.evaluate_expr(rst_val, ctx)?;
        let rst_val = generator.to_bitvec(ctx.module_id, rst_val);
        let prim_ty = sig_ty.maybe_to_bitvec();

        let dff = generator.net_list.add(
            ctx.module_id,
            DFF::new(
                prim_ty,
                clk,
                rst,
                en,
                rst_val,
                None,
                if en.is_none() {
                    SymIdent::Dff
                } else {
                    SymIdent::DffEn
                },
            ),
        );

        let dff_out = generator.net_list[dff].only_one_out().node_out_id();
        let dff_out = generator.from_bitvec(ctx.module_id, dff_out, sig_ty);

        ctx.new_closure_inputs().push(dff_out);
        let comb = generator.evaluate_expr(comb, ctx)?;
        let comb_out = generator.to_bitvec(ctx.module_id, comb);

        generator.net_list.set_dff_data(dff, comb_out);

        Ok(dff_out)
    }
}

pub struct SignalLift;

impl<'tcx> EvaluateExpr<'tcx> for SignalLift {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as arg);

        generator.evaluate_expr(arg, ctx)
    }
}

pub struct SignalMap;

impl<'tcx> EvaluateExpr<'tcx> for SignalMap {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec, comb);

        let rec = generator.evaluate_expr(rec, ctx)?;
        ctx.new_closure_inputs().push(rec);
        let comb = generator.evaluate_expr(comb, ctx)?;

        Ok(comb)
    }
}

pub struct SignalAndThen;

impl<'tcx> EvaluateExpr<'tcx> for SignalAndThen {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec, comb);

        let rec = generator.evaluate_expr(rec, ctx)?;
        ctx.new_closure_inputs().push(rec);
        let comb = generator.evaluate_expr(comb, ctx)?;

        Ok(comb)
    }
}

pub struct SignalApply2;

impl<'tcx> EvaluateExpr<'tcx> for SignalApply2 {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as arg1, arg2, comb);

        let arg1 = generator.evaluate_expr(arg1, ctx)?;
        let arg2 = generator.evaluate_expr(arg2, ctx)?;
        ctx.new_closure_inputs().push(arg1).push(arg2);
        let comb = generator.evaluate_expr(comb, ctx)?;

        Ok(comb)
    }
}

pub struct SignalValue;

impl<'tcx> EvaluateExpr<'tcx> for SignalValue {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.evaluate_expr(rec, ctx)
    }
}

pub struct SignalWatch;

impl<'tcx> EvaluateExpr<'tcx> for SignalWatch {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.evaluate_expr(rec, ctx)
    }
}

pub struct SignalOp {
    pub op: BinOp,
}

impl<'tcx> EvaluateExpr<'tcx> for SignalOp {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as lhs, rhs);
        let ty = generator.node_type(expr.hir_id, ctx);

        generator.bin_op(ty, self.op, lhs, rhs, ctx, expr.span)
    }
}
