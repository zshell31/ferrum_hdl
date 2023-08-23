use ferrum::{
    bit::{bit_value, Bit},
    prim_ty::PrimTy,
    unsigned::{unsigned_value, Unsigned},
};
use ferrum_netlist::{
    module::Module,
    net_list::NodeId,
    node::{DFFNode, Node, Splitter},
};
use rustc_ast::LitKind;
use rustc_hir::{
    definitions::{DefPath, DefPathDataName},
    Expr, ExprKind, Lit, QPath,
};
use rustc_middle::ty::{GenericArg, List};
use rustc_span::Span;

use crate::{
    error::{Error, SpanError, SpanErrorKind},
    generator::{Generator, Generic, Generics, Key},
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
    SignalMap,
    BitPackMsb,
    Conversion,
}

pub fn find_blackbox(def_path: &DefPath) -> Option<Blackbox> {
    // TODO: check crate
    if def_path == &ItemPath(&["signal", "reg"]) {
        #[allow(unused_imports)]
        use ferrum::signal::reg;
        return Some(Blackbox::RegisterFn);
    }

    if def_path == &ItemPath(&["signal", "Signal", "map"]) {
        #[allow(unused_imports)]
        use ferrum::signal::Signal;
        // TODO: check that map exists
        return Some(Blackbox::SignalMap);
    }

    if def_path == &ItemPath(&["bit_pack", "BitPack", "msb"]) {
        #[allow(unused_imports)]
        use ferrum::bit_pack::BitPack;
        // TODO: check that map exists
        return Some(Blackbox::BitPackMsb);
    }

    if def_path == &ItemPath(&["convert", "From", "from"]) {
        return Some(Blackbox::Conversion);
    }

    if def_path == &ItemPath(&["convert", "Into", "into"]) {
        return Some(Blackbox::Conversion);
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

    // if def_path == &ItemPath(&["signal", "MapSignal"]) {
    //     #[allow(unused_imports)]
    //     use ferrum::signal::MapSignal;
    //     return match key.generics {
    //         Some(Generics::G2(Generic::Ty(ty), _)) => {
    //             let def_id = ty.def_id()?;
    //             let def_path = tcx.def_path(def_id);
    //             find_prim_ty(tcx, &ty.ty()?.as_key(tcx, None), &def_path)
    //         }
    //         _ => None,
    //     };
    // }

    // if def_path == &ItemPath(&["signal", "Register"]) {
    //     #[allow(unused_imports)]
    //     use ferrum::signal::Register;
    //     return match key.generics {
    //         Some(Generics::G2(_, Generic::Ty(ty))) => {
    //             let def_id = ty.def_id()?;
    //             let def_path = tcx.def_path(def_id);
    //             find_prim_ty(tcx, &ty.ty()?.as_key(tcx, None), &def_path)
    //         }
    //         _ => None,
    //     };
    // }

    None
}

impl Blackbox {
    pub fn evaluate_expr<'tcx>(
        &self,
        generator: &mut Generator<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        expr: &Expr<'tcx>,
        module: &mut Module,
    ) -> Result<NodeId, Error> {
        match self {
            Self::RegisterFn => {
                RegisterFn::evaluate_expr(generator, generics, expr, module)
            }
            Self::SignalMap => {
                SignalMap::evaluate_expr(generator, generics, expr, module)
            }
            Self::BitPackMsb => {
                BitPackMsb::evaluate_expr(generator, generics, expr, module)
            }
            Self::Conversion => {
                Conversion::evaluate_expr(generator, generics, expr, module)
            }
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
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        expr: &Expr<'tcx>,
        module: &mut Module,
    ) -> Result<NodeId, Error> {
        let (rec, args) = utils::expected_call(expr)?;

        let ty = generator.node_type(rec.hir_id);

        let signal_val_ty = generator
            .generic_type(&ty, 1)
            .and_then(|ty| generator.find_prim_ty(&ty, generics, rec.span).ok());

        let gen = generator
            .generic_type(&ty, 1)
            .ok_or_else(|| Self::make_err(rec.span))?;
        let prim_ty = generator.find_prim_ty(&gen, generics, rec.span)?;

        let clk = generator.evaluate_expr(&args[0], generics, module)?;
        let rst_value = generator.evaluate_expr(&args[1], generics, module)?;
        let comb = generator.evaluate_expr(&args[2], generics, module)?;

        if let Node::Const(node) = module.net_list.node_mut(rst_value) {
            node.inject = true;
            // Add conversion from node.out.ty to signal_val_ty
            if let Some(prim_ty) = signal_val_ty {
                node.out.ty = prim_ty;
            }
        }

        let dff = module.net_list.add_node(DFFNode::new(
            prim_ty,
            clk,
            rst_value,
            None,
            generator.idents.tmp(),
        ));

        module.net_list.link_dummy_input(comb, dff);
        module.net_list.link_dff(dff, comb);

        if let Node::DFF(dff) = module.net_list.node_mut(dff) {
            dff.data = Some(comb);
        }

        Ok(dff)
    }
}

struct SignalMap;

impl SignalMap {
    pub fn evaluate_expr<'tcx>(
        generator: &mut Generator<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        expr: &Expr<'tcx>,
        module: &mut Module,
    ) -> Result<NodeId, Error> {
        let (_, rec, args, _) = utils::exptected_method_call(expr)?;

        let rec = generator.evaluate_expr(rec, generics, module)?;
        let comb = generator.evaluate_expr(&args[0], generics, module)?;

        module.net_list.link_dummy_input(comb, rec);

        Ok(comb)
    }
}

struct BitPackMsb;

impl BitPackMsb {
    pub fn evaluate_expr<'tcx>(
        generator: &mut Generator<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        expr: &Expr<'tcx>,
        module: &mut Module,
    ) -> Result<NodeId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;

        let rec = generator.evaluate_expr(rec, generics, module)?;

        let start = module.net_list.node_output(rec).ty.width() - 1;
        let split = module.net_list.add_node(Splitter::new(
            PrimTy::Bit,
            rec,
            start,
            1,
            generator.idents.tmp(),
        ));

        Ok(split)
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
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        expr: &Expr<'tcx>,
        module: &mut Module,
    ) -> Result<NodeId, Error> {
        match expr.kind {
            ExprKind::Call(rec, args) => {
                let from = generator.find_prim_ty(
                    &generator.node_type(args[0].hir_id),
                    generics,
                    args[0].span,
                )?;
                let target = match rec.kind {
                    ExprKind::Path(QPath::TypeRelative(ty, _)) => generator
                        .find_prim_ty(
                            &generator.node_type(ty.hir_id),
                            generics,
                            rec.span,
                        )?,
                    _ => {
                        return Err(Self::make_err(rec.span));
                    }
                };

                Self::convert(from, target, generator, generics, &args[0], module)
            }
            ExprKind::MethodCall(_, rec, _, span) => {
                let from = generator.find_prim_ty(
                    &generator.node_type(rec.hir_id),
                    generics,
                    rec.span,
                )?;
                let target = generator.find_prim_ty(
                    &generator.node_type(expr.hir_id),
                    generics,
                    span,
                )?;

                Self::convert(from, target, generator, generics, rec, module)
            }
            _ => Err(Self::make_err(expr.span)),
        }
    }

    fn convert<'tcx>(
        from: PrimTy,
        target: PrimTy,
        generator: &mut Generator<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        expr: &Expr<'tcx>,
        module: &mut Module,
    ) -> Result<NodeId, Error> {
        match (from, target) {
            (PrimTy::Bool, PrimTy::Bit) => {
                assert_convert::<bool, Bit>();
                generator.evaluate_expr(expr, generics, module)
            }
            (PrimTy::Bit, PrimTy::Bool) => {
                assert_convert::<Bit, bool>();
                generator.evaluate_expr(expr, generics, module)
            }
            (PrimTy::U128, PrimTy::Unsigned(n)) => {
                assert_convert::<u128, Unsigned<1>>();
                let node_id = generator.evaluate_expr(expr, generics, module)?;
                let node = module.net_list.node_mut(node_id);
                node.node_output_mut(0).ty = PrimTy::Unsigned(n);
                Ok(node_id)
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
