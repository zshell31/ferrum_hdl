use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{
    group::ItemId,
    node::{BinOp, DFF},
    symbol::Symbol,
};
use rustc_hir::Expr;
use rustc_span::Span;

use super::EvalExpr;
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
                BlackboxKind::SignalReg
            } else {
                BlackboxKind::SignalRegEn
            }),
            span,
        )
        .into()
    }
}

impl<'tcx> EvalExpr<'tcx> for SignalReg {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let args = utils::expected_call(expr)?.args;

        let ty = generator.node_type(expr.hir_id, ctx);
        let value_ty =
            utils::subst_type(ty, 1).ok_or_else(|| self.make_err(expr.span))?;
        let sig_ty = generator.find_sig_ty(value_ty, ctx, expr.span)?;
        let node_ty = sig_ty.to_bitvec();

        let (clk, rst, en, rst_val, comb) = match self.has_en {
            true => (&args[0], &args[1], Some(&args[2]), &args[3], &args[4]),
            false => (&args[0], &args[1], None, &args[2], &args[3]),
        };

        let clk = generator.eval_expr(clk, ctx)?.node_out_id();

        let rst = generator.eval_expr(rst, ctx)?;
        let rst = generator.to_bitvec(ctx.module_id, rst);

        let rst_val = generator.eval_expr(rst_val, ctx)?;
        let rst_val = generator.to_bitvec(ctx.module_id, rst_val);

        let en = en
            .map(|en| {
                generator
                    .eval_expr(en, ctx)
                    .map(|item_id| item_id.node_out_id())
            })
            .transpose()?;

        let closure_id =
            generator.eval_closure(comb, Symbol::new("SignalReg"), true, ctx)?;

        let dff = generator.netlist.add(
            ctx.module_id,
            DFF::new(
                node_ty,
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

        let dff_out = generator.netlist[dff].only_one_out().node_out_id();
        let dff_out = generator.from_bitvec(ctx.module_id, dff_out, sig_ty);

        let comb = generator.instantiate_closure(closure_id, [dff_out], sig_ty, ctx)?;
        let comb_out = generator.to_bitvec(ctx.module_id, comb);

        generator.netlist.set_dff_data(dff, comb_out);

        Ok(dff_out)
    }
}

pub struct SignalLift;

impl<'tcx> EvalExpr<'tcx> for SignalLift {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as arg);

        generator.eval_expr(arg, ctx)
    }
}

pub struct SignalMap;

impl<'tcx> EvalExpr<'tcx> for SignalMap {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec, comb);

        let output_ty = generator.node_type(expr.hir_id, ctx);
        let output_ty = generator.find_sig_ty(output_ty, ctx, expr.span)?;

        let rec = generator.eval_expr(rec, ctx)?;

        let closure_id =
            generator.eval_closure(comb, Symbol::new("SignalMap"), true, ctx)?;
        generator.instantiate_closure(closure_id, [rec], output_ty, ctx)
    }
}

pub struct SignalAndThen;

impl<'tcx> EvalExpr<'tcx> for SignalAndThen {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec, comb);

        let output_ty = generator.node_type(expr.hir_id, ctx);
        let output_ty = generator.find_sig_ty(output_ty, ctx, expr.span)?;

        let rec = generator.eval_expr(rec, ctx)?;

        let closure_id =
            generator.eval_closure(comb, Symbol::new("SignalAndThen"), true, ctx)?;
        generator.instantiate_closure(closure_id, [rec], output_ty, ctx)
    }
}

pub struct SignalApply2;

impl<'tcx> EvalExpr<'tcx> for SignalApply2 {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as arg1, arg2, comb);

        let output_ty = generator.node_type(expr.hir_id, ctx);
        let output_ty = generator.find_sig_ty(output_ty, ctx, expr.span)?;

        let arg1 = generator.eval_expr(arg1, ctx)?;
        let arg2 = generator.eval_expr(arg2, ctx)?;

        let closure_id =
            generator.eval_closure(comb, Symbol::new("SignalApply2"), true, ctx)?;
        generator.instantiate_closure(closure_id, [arg1, arg2], output_ty, ctx)
    }
}

pub struct SignalValue;

impl<'tcx> EvalExpr<'tcx> for SignalValue {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.eval_expr(rec, ctx)
    }
}

pub struct SignalWatch;

impl<'tcx> EvalExpr<'tcx> for SignalWatch {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        utils::args!(expr as rec);

        generator.eval_expr(rec, ctx)
    }
}

pub struct SignalOp {
    pub op: BinOp,
}

impl<'tcx> EvalExpr<'tcx> for SignalOp {
    fn eval_expr(
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
