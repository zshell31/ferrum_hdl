use ferrum_blackbox::Blackbox;
use ferrum_netlist::{group::ItemId, node::DFF, params::Outputs};
use rustc_hir::Expr;
use rustc_span::Span;

use super::EvaluateExpr;
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    generator::{EvalContext, Generator},
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
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (rec, args) = utils::expected_call(expr)?;

        let ty = generator.node_type(rec.hir_id, ctx);

        let value_ty = utils::subst_type(ty, 1).ok_or_else(|| self.make_err(rec.span))?;
        let sig_ty = generator.find_sig_ty(value_ty, ctx.generic_args, rec.span)?;

        let (clk, rst, en, rst_val, comb) = match self.has_en {
            true => (&args[0], &args[1], Some(&args[2]), &args[3], &args[4]),
            false => (&args[0], &args[1], None, &args[2], &args[3]),
        };

        let clk = generator.evaluate_expr(clk, ctx)?.node_id();

        let rst = generator
            .evaluate_expr(rst, ctx)
            .map(|item_id| item_id.node_id())
            .map(|node_id| {
                generator.net_list[node_id]
                    .outputs()
                    .only_one()
                    .node_out_id(node_id)
            })?;

        let en = en
            .map(|en| {
                generator
                    .evaluate_expr(en, ctx)
                    .map(|item_id| item_id.node_id())
                    .map(|node_id| {
                        generator.net_list[node_id]
                            .outputs()
                            .only_one()
                            .node_out_id(node_id)
                    })
            })
            .transpose()?;

        let rst_val = generator.evaluate_expr(rst_val, ctx)?;
        let rst_val = generator.maybe_to_bitvec(ctx.module_id, rst_val);

        let comb = generator.evaluate_expr(comb, ctx)?;

        let comb_out = generator.maybe_to_bitvec(ctx.module_id, comb);

        let prim_ty = sig_ty.maybe_to_bitvec();

        let dff = generator.net_list.add_node(
            ctx.module_id,
            DFF::new(
                prim_ty,
                generator.net_list.only_one_node_out_id(clk),
                rst,
                en,
                rst_val,
                comb_out,
                generator.idents.for_module(ctx.module_id).tmp(),
            ),
        );
        let dff_out = generator.net_list[dff]
            .outputs()
            .only_one()
            .node_out_id(dff);

        let dff_out = generator.maybe_from_bitvec(ctx.module_id, dff_out, sig_ty);

        generator.link_dummy_inputs(&[dff_out], comb, rec.span)?;

        Ok(dff_out)
    }
}

pub struct SignalLift;

impl<'tcx> EvaluateExpr<'tcx> for SignalLift {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, args) = utils::expected_call(expr)?;

        generator.evaluate_expr(&args[0], ctx)
    }
}

pub struct SignalMap;

impl<'tcx> EvaluateExpr<'tcx> for SignalMap {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, args, _) = utils::exptected_method_call(expr)?;
        let span = rec.span;

        let rec = generator.evaluate_expr(rec, ctx)?;
        let comb = generator.evaluate_expr(&args[0], ctx)?;

        generator.link_dummy_inputs(&[rec], comb, span)?;

        Ok(comb)
    }
}

pub struct SignalAndThen;

impl<'tcx> EvaluateExpr<'tcx> for SignalAndThen {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, args, _) = utils::exptected_method_call(expr)?;
        let span = rec.span;

        let rec = generator.evaluate_expr(rec, ctx)?;
        let comb = generator.evaluate_expr(&args[0], ctx)?;

        generator.link_dummy_inputs(&[rec], comb, span)?;

        Ok(comb)
    }
}

pub struct SignalApply2;

impl<'tcx> EvaluateExpr<'tcx> for SignalApply2 {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (rec, args) = utils::expected_call(expr)?;

        let arg1 = generator.evaluate_expr(&args[0], ctx)?;
        let arg2 = generator.evaluate_expr(&args[1], ctx)?;
        let comb = generator.evaluate_expr(&args[2], ctx)?;

        generator.link_dummy_inputs(&[arg1, arg2], comb, rec.span)?;

        Ok(comb)
    }
}

pub struct SignalValue;

impl<'tcx> EvaluateExpr<'tcx> for SignalValue {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;

        generator.evaluate_expr(rec, ctx)
    }
}

pub struct SignalWatch;

impl<'tcx> EvaluateExpr<'tcx> for SignalWatch {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;

        generator.evaluate_expr(rec, ctx)
    }
}
