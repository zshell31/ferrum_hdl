use ferrum::{
    bit::Bit,
    prim_ty::{PrimTy, PrimValue},
};
use ferrum_netlist::{
    index::NodeIndex,
    node::{DFFNode, Node, PassNode},
};
use rustc_ast::LitKind;
use rustc_hir::{
    definitions::{DefPath, DefPathDataName},
    Expr, ExprKind, Lit, QPath,
};
use rustc_span::Span;

use crate::{
    error::{Error, SpanError, SpanErrorKind},
    generator::Generator,
    utils,
};

pub struct ItemPath(&'static [&'static str]);

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
    Conversion,
}

pub fn find_blackbox(def_path: &DefPath) -> Option<Blackbox> {
    // TODO: check crate
    if def_path == &ItemPath(&["signal", "register"]) {
        #[allow(unused_imports)]
        use ferrum::signal::register;
        return Some(Blackbox::RegisterFn);
    }

    if def_path == &ItemPath(&["convert", "From", "from"]) {
        return Some(Blackbox::Conversion);
    }

    if def_path == &ItemPath(&["convert", "Into", "into"]) {
        return Some(Blackbox::Conversion);
    }

    None
}

pub fn find_prim_ty(def_path: &DefPath) -> Option<PrimTy> {
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

    None
}

impl Blackbox {
    pub fn evaluate_expr<'tcx>(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
    ) -> Result<NodeIndex, Error> {
        match self {
            Self::RegisterFn => RegisterFn::evaluate_expr(generator, expr),
            Self::Conversion => Conversion::evaluate_expr(generator, expr),
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

    fn evaluate_expr<'tcx>(
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
    ) -> Result<NodeIndex, Error> {
        let (rec, args) = utils::expected_call(expr)?;

        let ty = generator.node_type(rec.hir_id);
        let gen = generator
            .generic_type(&ty, 1)
            .ok_or_else(|| Self::make_err(rec.span))?;
        let prim_ty = generator.find_prim_ty(gen, rec.span)?;

        let clk = generator.evaluate_expr(&args[0])?;
        let rst_value = generator.evaluate_expr(&args[1])?;

        let dff = generator.net_list.add_node(DFFNode::new(
            prim_ty,
            clk,
            rst_value,
            None,
            generator.idents.tmp(),
        ));

        let comb = generator.evaluate_expr(&args[2])?;

        let dummy = generator
            .net_list
            .find_dummy_inputs(comb)
            .first()
            .copied()
            .ok_or_else(|| Self::make_err(rec.span))?;
        let dummy_out = generator.net_list.node_output(dummy);

        generator
            .net_list
            .replace(dummy, PassNode::new(dummy_out.ty, dff, dummy_out.sym));

        if let Node::DFF(dff) = generator.net_list.node_mut(dff) {
            dff.data = Some(comb);
        }

        Ok(dff)
    }
}

struct Conversion;

impl Conversion {
    fn make_err(span: Span) -> Error {
        SpanError::new(
            SpanErrorKind::NotSynthBlackboxExpr(Blackbox::RegisterFn),
            span,
        )
        .into()
    }

    pub fn evaluate_expr<'tcx>(
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
    ) -> Result<NodeIndex, Error> {
        match expr.kind {
            ExprKind::Call(rec, args) => {
                let from = generator
                    .find_prim_ty(generator.node_type(args[0].hir_id), args[0].span)?;
                let target = match rec.kind {
                    ExprKind::Path(QPath::TypeRelative(ty, _)) => generator
                        .find_prim_ty(generator.node_type(ty.hir_id), rec.span)?,
                    _ => {
                        return Err(Self::make_err(rec.span));
                    }
                };

                Self::convert(from, target, generator, &args[0])
            }
            ExprKind::MethodCall(_, rec, _, span) => {
                let from =
                    generator.find_prim_ty(generator.node_type(rec.hir_id), rec.span)?;
                let target =
                    generator.find_prim_ty(generator.node_type(expr.hir_id), span)?;

                Self::convert(from, target, generator, rec)
            }
            _ => Err(Self::make_err(expr.span)),
        }
    }

    fn convert<'tcx>(
        from: PrimTy,
        target: PrimTy,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
    ) -> Result<NodeIndex, Error> {
        match (from, target) {
            (PrimTy::Bool, PrimTy::Bit) => {
                assert_convert::<bool, Bit>();
                generator.evaluate_expr(expr)
            }
            (PrimTy::Bit, PrimTy::Bool) => {
                assert_convert::<Bit, bool>();
                generator.evaluate_expr(expr)
            }
            _ => Err(
                SpanError::new(SpanErrorKind::UnsupportedConversion, expr.span).into(),
            ),
        }
    }
}

fn assert_convert<F, T: From<F>>() {}

pub fn evaluate_lit(prim_ty: PrimTy, lit: &Lit) -> Result<u128, Error> {
    match prim_ty {
        PrimTy::Bool => evaluate_bit_lit(lit),
        PrimTy::Bit => evaluate_bit_lit(lit),
        PrimTy::Clock => Err(SpanError::new(
            SpanErrorKind::PrimTyWithoutValue(PrimTy::Clock),
            lit.span,
        )
        .into()),
    }
}

fn evaluate_bit_lit(lit: &Lit) -> Result<u128, Error> {
    match lit.node {
        LitKind::Bool(bool) => Ok(Bit::from(bool).value()),
        _ => Err(SpanError::new(
            SpanErrorKind::UnexpectedLitValue(PrimTy::Bit),
            lit.span,
        )
        .into()),
    }
}
