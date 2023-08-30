use ferrum::{
    bit::{bit_value, Bit},
    prim_ty::PrimTy,
    unsigned::{unsigned_value, Unsigned},
};
use ferrum_netlist::{
    group_list::ItemId,
    node::{DFFNode, IsNode, Node, Splitter},
    params::Outputs,
};
use rustc_ast::LitKind;
use rustc_hir::{
    definitions::{DefPath, DefPathDataName},
    Expr, ExprKind, Lit, QPath,
};
use rustc_span::Span;

use crate::{
    error::{Error, SpanError, SpanErrorKind},
    generator::{EvalContext, Generator, Generic, Generics, Key},
    utils,
};

pub struct ItemPath(pub &'static [&'static str]);

impl PartialEq<ItemPath> for DefPath {
    fn eq(&self, other: &ItemPath) -> bool {
        if self.data.len() != other.0.len() {
            false
        } else {
            self.data
                .iter()
                .zip(other.0.iter())
                .all(|(def_path, &block_path)| match def_path.data.name() {
                    DefPathDataName::Named(name) => name.as_str() == block_path,
                    _ => false,
                })
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Blackbox {
    RegisterFn,
    SignalMap,
    SignalApply2,
    BitPackMsb,
    StdConversion,
    StdClone,
}

pub fn find_blackbox(def_path: &DefPath) -> Option<Blackbox> {
    // TODO: check crate
    if def_path == &ItemPath(&["signal", "reg"]) {
        #[allow(unused_imports)]
        use ferrum::signal::reg;
        return Some(Blackbox::RegisterFn);
    }

    if def_path == &ItemPath(&["signal", "Signal", "smap"]) {
        #[allow(unused_imports)]
        use ferrum::signal::Signal;
        // TODO: check that map exists
        return Some(Blackbox::SignalMap);
    }

    if def_path == &ItemPath(&["signal", "apply2"]) {
        #[allow(unused_imports)]
        use ferrum::signal::apply2;
        // TODO: check that map exists
        return Some(Blackbox::SignalApply2);
    }

    if def_path == &ItemPath(&["bit_pack", "BitPack", "msb"]) {
        #[allow(unused_imports)]
        use ferrum::bit_pack::BitPack;
        // TODO: check that msb exists
        return Some(Blackbox::BitPackMsb);
    }

    if def_path == &ItemPath(&["convert", "From", "from"]) {
        return Some(Blackbox::StdConversion);
    }

    if def_path == &ItemPath(&["convert", "Into", "into"]) {
        return Some(Blackbox::StdConversion);
    }

    if def_path == &ItemPath(&["clone", "Clone", "clone"]) {
        return Some(Blackbox::StdClone);
    }

    None
}

pub fn find_prim_ty(key: &Key<'_>, def_path: &DefPath) -> Option<PrimTy> {
    // TODO: check crate
    if def_path == &ItemPath(&["bit", "Bit"]) {
        #[allow(unused_imports)]
        use ferrum::bit::Bit;
        return Some(PrimTy::Bit);
    }

    if def_path == &ItemPath(&["signal", "Clock"]) {
        #[allow(unused_imports)]
        use ferrum::signal::Clock;
        return Some(PrimTy::Clock);
    }

    if def_path == &ItemPath(&["unsigned", "Unsigned"]) {
        #[allow(unused_imports)]
        use ferrum::unsigned::Unsigned;
        return match key.generics {
            Some(Generics::G1(Generic::Const(val))) => Some(PrimTy::Unsigned(val)),
            _ => None,
        };
    }

    None
}

pub trait EvaluateExpr<'tcx> {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error>;
}

impl<'tcx> EvaluateExpr<'tcx> for Blackbox {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        match self {
            Self::RegisterFn => RegisterFn.evaluate_expr(generator, expr, ctx),
            Self::SignalMap => SignalMap.evaluate_expr(generator, expr, ctx),
            Self::SignalApply2 => SignalApply2.evaluate_expr(generator, expr, ctx),
            Self::BitPackMsb => BitPackMsb.evaluate_expr(generator, expr, ctx),
            Self::StdConversion => StdConversion.evaluate_expr(generator, expr, ctx),
            Self::StdClone => StdClone.evaluate_expr(generator, expr, ctx),
        }
    }
}

struct RegisterFn;

impl RegisterFn {
    fn make_err(span: Span) -> Error {
        SpanError::new(
            SpanErrorKind::NotSynthBlackboxExpr(Blackbox::RegisterFn),
            span,
        )
        .into()
    }
}

impl<'tcx> EvaluateExpr<'tcx> for RegisterFn {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (rec, args) = utils::expected_call(expr)?;

        let ty = generator.node_type(rec.hir_id);

        let signal_val_ty = generator
            .generic_type(&ty, 1)
            .and_then(|ty| generator.find_prim_ty(&ty, ctx.generics, rec.span).ok());

        let gen = generator
            .generic_type(&ty, 1)
            .ok_or_else(|| Self::make_err(rec.span))?;
        let prim_ty = generator.find_prim_ty(&gen, ctx.generics, rec.span)?;

        let clk = generator.evaluate_expr(&args[0], ctx)?.node_id();
        let rst_value = generator.evaluate_expr(&args[1], ctx)?.node_id();
        let comb = generator.evaluate_expr(&args[2], ctx)?.node_id();

        if let Node::Const(node) = &mut generator.net_list[rst_value] {
            node.inject = true;
            // Add conversion from node.out.ty to signal_val_ty
            if let Some(prim_ty) = signal_val_ty {
                node.output.ty = prim_ty;
            }
        }

        let dff = generator.net_list.add_node(
            ctx.module_id,
            DFFNode::new(
                prim_ty,
                generator.net_list.only_one_node_out_id(clk),
                generator.net_list.only_one_node_out_id(rst_value),
                generator.net_list.only_one_node_out_id(comb),
                generator.idents.tmp(),
            ),
        );

        let dff_out = generator.net_list.only_one_node_out_id(dff);

        generator.net_list.link_dummy_input(comb, &[dff_out], true);

        Ok(dff.into())
    }
}

struct SignalMap;

impl<'tcx> EvaluateExpr<'tcx> for SignalMap {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, args, _) = utils::exptected_method_call(expr)?;

        let rec = generator.evaluate_expr(rec, ctx)?.node_id();
        let comb = generator.evaluate_expr(&args[0], ctx)?.node_id();

        let rec = generator.net_list.only_one_node_out_id(rec);
        generator.net_list.link_dummy_input(comb, &[rec], false);

        Ok(comb.into())
    }
}

struct SignalApply2;

impl<'tcx> EvaluateExpr<'tcx> for SignalApply2 {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, args) = utils::expected_call(expr)?;

        let arg1 = generator.evaluate_expr(&args[0], ctx)?.node_id();
        let arg2 = generator.evaluate_expr(&args[1], ctx)?.node_id();
        let comb = generator.evaluate_expr(&args[2], ctx)?.node_id();

        let arg1 = generator.net_list.only_one_node_out_id(arg1);
        let arg2 = generator.net_list.only_one_node_out_id(arg2);
        generator
            .net_list
            .link_dummy_input(comb, &[arg1, arg2], false);

        Ok(comb.into())
    }
}

struct BitPackMsb;

impl<'tcx> EvaluateExpr<'tcx> for BitPackMsb {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;

        let rec = generator.evaluate_expr(rec, ctx)?.node_id();

        let start = generator.net_list[rec].outputs().only_one().out.ty.width() - 1;
        let rec = generator.net_list.only_one_node_out_id(rec);

        let split = generator.net_list.add_node(
            ctx.module_id,
            Splitter::new(PrimTy::Bit, rec, start, 1, generator.idents.tmp()),
        );

        Ok(split.into())
    }
}

struct StdConversion;

impl StdConversion {
    fn make_err(span: Span) -> Error {
        SpanError::new(
            SpanErrorKind::NotSynthBlackboxExpr(Blackbox::RegisterFn),
            span,
        )
        .into()
    }

    fn convert<'tcx>(
        from: PrimTy,
        target: PrimTy,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        match (from, target) {
            (PrimTy::Bool, PrimTy::Bit) => {
                assert_convert::<bool, Bit>();
                generator.evaluate_expr(expr, ctx)
            }
            (PrimTy::Bit, PrimTy::Bool) => {
                assert_convert::<Bit, bool>();
                generator.evaluate_expr(expr, ctx)
            }
            (PrimTy::U128, PrimTy::Unsigned(n)) => {
                assert_convert::<u128, Unsigned<1>>();
                let node_id = generator.evaluate_expr(expr, ctx)?;

                let node_out = generator.net_list[node_id.node_id()]
                    .outputs_mut()
                    .only_one_mut()
                    .out;
                node_out.ty = PrimTy::Unsigned(n);

                Ok(node_id)
            }
            _ => Err(
                SpanError::new(SpanErrorKind::UnsupportedConversion, expr.span).into(),
            ),
        }
    }
}

fn assert_convert<F, T: From<F>>() {}

impl<'tcx> EvaluateExpr<'tcx> for StdConversion {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        match expr.kind {
            ExprKind::Call(rec, args) => {
                let from = generator.find_prim_ty(
                    &generator.node_type(args[0].hir_id),
                    ctx.generics,
                    args[0].span,
                )?;
                let target = match rec.kind {
                    ExprKind::Path(QPath::TypeRelative(ty, _)) => generator
                        .find_prim_ty(
                            &generator.node_type(ty.hir_id),
                            ctx.generics,
                            rec.span,
                        )?,
                    _ => {
                        return Err(Self::make_err(rec.span));
                    }
                };

                Self::convert(from, target, generator, &args[0], ctx)
            }
            ExprKind::MethodCall(_, rec, _, span) => {
                let from = generator.find_prim_ty(
                    &generator.node_type(rec.hir_id),
                    ctx.generics,
                    rec.span,
                )?;
                let target = generator.find_prim_ty(
                    &generator.node_type(expr.hir_id),
                    ctx.generics,
                    span,
                )?;

                Self::convert(from, target, generator, rec, ctx)
            }
            _ => Err(Self::make_err(expr.span)),
        }
    }
}

struct StdClone;

impl<'tcx> EvaluateExpr<'tcx> for StdClone {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;

        generator.evaluate_expr(rec, ctx)
    }
}

pub fn evaluate_lit(prim_ty: PrimTy, lit: &Lit) -> Result<u128, Error> {
    match prim_ty {
        PrimTy::Bool => evaluate_bit_lit(lit),
        PrimTy::Bit => evaluate_bit_lit(lit),
        PrimTy::U128 => evaluate_unsigned_lit(lit, prim_ty.width()),
        PrimTy::Unsigned(n) => evaluate_unsigned_lit(lit, n),
        PrimTy::Clock => Err(SpanError::new(
            SpanErrorKind::PrimTyWithoutValue(PrimTy::Clock),
            lit.span,
        )
        .into()),
    }
}

fn evaluate_bit_lit(lit: &Lit) -> Result<u128, Error> {
    match lit.node {
        LitKind::Bool(b) => Ok(bit_value(b)),
        _ => Err(SpanError::new(
            SpanErrorKind::UnexpectedLitValue(PrimTy::Bit),
            lit.span,
        )
        .into()),
    }
}

fn evaluate_unsigned_lit(lit: &Lit, width: u8) -> Result<u128, Error> {
    match lit.node {
        LitKind::Int(n, _) => Ok(unsigned_value(n, width)),
        _ => Err(SpanError::new(
            SpanErrorKind::UnexpectedLitValue(PrimTy::Unsigned(width)),
            lit.span,
        )
        .into()),
    }
}
