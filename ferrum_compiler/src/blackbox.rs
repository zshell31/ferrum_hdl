use ferrum::{
    bit::Bit,
    prim_ty::{PrimTy, PrimValue},
};
use ferrum_netlist::{
    index::NodeIndex,
    node::{DFFNode, Node, Pass, PassNode, DFF},
};
use rustc_ast::LitKind;
use rustc_hir::{
    definitions::{DefPath, DefPathDataName},
    Expr, ExprKind, Lit,
};

use crate::{
    error::{Error, SpanError, SpanErrorKind},
    generator::Generator,
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
}

pub fn find_blackbox(def_path: &DefPath) -> Option<Blackbox> {
    if def_path == &ItemPath(&["signal", "register"]) {
        #[allow(unused_imports)]
        use ferrum::signal::register;
        return Some(Blackbox::RegisterFn);
    }

    None
}

pub fn find_prim_ty(def_path: &DefPath) -> Option<PrimTy> {
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
        }
    }
}

struct RegisterFn;

impl RegisterFn {
    fn evaluate_expr<'tcx>(
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
    ) -> Result<NodeIndex, Error> {
        let make_err = |span| {
            Error::from(SpanError::new(
                SpanErrorKind::NotSynthBlackboxExpr(Blackbox::RegisterFn),
                span,
            ))
        };

        match expr.kind {
            ExprKind::Call(rec, args) if args.len() == 3 => {
                let ty = generator.node_type(rec.hir_id);
                let gen = generator
                    .generic_type(&ty, 1)
                    .ok_or_else(|| make_err(rec.span))?;
                let prim_ty = generator.find_prim_ty(gen, rec.span)?;

                let clk = generator.evaluate_expr(&args[0])?;
                let rst_value = generator.evaluate_expr(&args[1])?;

                let dff = generator.net_list.add_node::<DFF>(DFFNode::new(
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
                    .ok_or_else(|| make_err(rec.span))?;
                let dummy_out = generator.net_list.node_output(dummy);

                generator.net_list.replace::<Pass>(
                    dummy,
                    PassNode::new(dummy_out.ty, dff, dummy_out.sym),
                );

                if let Node::DFF(dff) = generator.net_list.node_mut(dff) {
                    dff.data = Some(comb);
                }

                Ok(dff)
            }
            _ => Err(make_err(expr.span)),
        }
    }
}

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
