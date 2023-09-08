use ferrum::{
    bit::{bit_value, Bit},
    unsigned::{unsigned_value, Unsigned},
};
use ferrum_netlist::{
    group_list::ItemId,
    net_list::{NodeId, NodeOutId},
    node::{BitVecTrans, DFFNode, IsNode, Node, Splitter},
    params::Outputs,
    sig_ty::{PrimTy, SignalTy},
};
use rustc_ast::LitKind;
use rustc_hir::{
    definitions::{DefPath, DefPathDataName},
    Expr, ExprKind, Lit, QPath,
};
use rustc_span::Span;

use crate::{
    bitvec::ArrayDesc,
    error::{Error, SpanError, SpanErrorKind},
    generator::{EvalContext, Generator, Key},
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
                    DefPathDataName::Anon { namespace } => {
                        namespace.as_str() == block_path
                    }
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
    ArrayIntoInner,
    ArrayReverse,
    Unbundle,
    Bundle,
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

    if def_path == &ItemPath(&["signal", "impl", "map"]) {
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

    if def_path == &ItemPath(&["array", "impl", "into_inner"]) {
        #[allow(unused_imports)]
        use ferrum::array::Array;
        return Some(Blackbox::ArrayIntoInner);
    }

    if def_path == &ItemPath(&["array", "impl", "reverse"]) {
        #[allow(unused_imports)]
        use ferrum::array::Array;
        return Some(Blackbox::ArrayReverse);
    }

    if def_path == &ItemPath(&["signal", "Bundle", "bundle"]) {
        return Some(Blackbox::Bundle);
    }

    if def_path == &ItemPath(&["signal", "Bundle", "unbundle"]) {
        return Some(Blackbox::Unbundle);
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

    println!("{:?}", def_path);
    None
}

pub fn find_sig_ty(key: &Key<'_>, def_path: &DefPath) -> Option<SignalTy> {
    // TODO: check crate
    if def_path == &ItemPath(&["signal", "Signal"]) {
        #[allow(unused_imports)]
        use ferrum::signal::Signal;
        return key.generic_ty(0);
    }

    if def_path == &ItemPath(&["bit", "Bit"]) {
        #[allow(unused_imports)]
        use ferrum::bit::Bit;
        return Some(PrimTy::Bit.into());
    }

    if def_path == &ItemPath(&["domain", "Clock"]) {
        #[allow(unused_imports)]
        use ferrum::domain::Clock;
        return Some(PrimTy::Clock.into());
    }

    if def_path == &ItemPath(&["unsigned", "Unsigned"]) {
        #[allow(unused_imports)]
        use ferrum::unsigned::Unsigned;

        return key.generic_const(0).map(|val| PrimTy::Unsigned(val).into());
    }

    if def_path == &ItemPath(&["array", "Array"]) {
        #[allow(unused_imports)]
        use ferrum::array::Array;

        let n = key.generic_const(0)?;
        let ty = key.generic_ty(1)?;

        return Some(SignalTy::array(n, ty));
    }

    println!("{:?}", def_path);

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
            Self::ArrayIntoInner => ArrayIntoInner.evaluate_expr(generator, expr, ctx),
            Self::ArrayReverse => ArrayReverse.evaluate_expr(generator, expr, ctx),
            Self::Unbundle => Unbundle.evaluate_expr(generator, expr, ctx),
            Self::Bundle => Bundle.evaluate_expr(generator, expr, ctx),
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

        let signal_val_ty = generator.generic_type(&ty, 1).and_then(|ty| {
            generator
                .find_sig_ty(&ty, ctx.generics, rec.span)
                .ok()
                .map(|sig_ty| sig_ty.prim_ty())
        });

        let gen = generator
            .generic_type(&ty, 1)
            .ok_or_else(|| Self::make_err(rec.span))?;
        let prim_ty = generator
            .find_sig_ty(&gen, ctx.generics, rec.span)?
            .prim_ty();

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
                generator.idents.for_module(ctx.module_id).tmp(),
            ),
        );

        generator.link_dummy_inputs(&[dff.into()], comb.into(), rec.span)?;

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
        let span = rec.span;

        let rec = generator.evaluate_expr(rec, ctx)?;
        let comb = generator.evaluate_expr(&args[0], ctx)?;

        generator.link_dummy_inputs(&[rec], comb, span)?;

        Ok(comb)
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
        let (rec, args) = utils::expected_call(expr)?;

        let arg1 = generator.evaluate_expr(&args[0], ctx)?;
        let arg2 = generator.evaluate_expr(&args[1], ctx)?;
        let comb = generator.evaluate_expr(&args[2], ctx)?;

        generator.link_dummy_inputs(&[arg1, arg2], comb, rec.span)?;

        Ok(comb)
    }
}

pub struct BitVecTransArgs {
    pub sig_ty: SignalTy,
    pub rec: ItemId,
    pub bit_vec: NodeOutId,
}

pub fn bit_vec_trans<'tcx>(
    generator: &mut Generator<'tcx>,
    rec: &Expr<'tcx>,
    ctx: EvalContext<'tcx>,
    trans: impl FnOnce(
        &mut Generator<'tcx>,
        &EvalContext<'tcx>,
        BitVecTransArgs,
    ) -> Result<(NodeId, SignalTy), Error>,
) -> Result<ItemId, Error> {
    let sig_ty = generator.find_sig_ty(
        &generator.node_type(rec.hir_id),
        ctx.generics,
        rec.span,
    )?;
    let rec = generator.evaluate_expr(rec, ctx)?;

    let to = generator.to_bitvec(ctx.module_id, rec);

    let args = BitVecTransArgs {
        sig_ty,
        rec,
        bit_vec: to,
    };
    let (trans, sig_ty) = trans(generator, &ctx, args)?;
    let trans = generator.net_list[trans]
        .outputs()
        .only_one()
        .node_out_id(trans);

    let from = generator.from_bitvec(ctx.module_id, trans, sig_ty);

    Ok(from)
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

        bit_vec_trans(
            generator,
            rec,
            ctx,
            |generator, ctx, BitVecTransArgs { bit_vec, .. }| {
                let start = generator.net_list[bit_vec].ty.width() - 1;
                let ty = PrimTy::Bit;

                Ok((
                    generator.net_list.add_node(
                        ctx.module_id,
                        Splitter::new(
                            bit_vec,
                            [(ty, generator.idents.for_module(ctx.module_id).tmp())],
                            Some(start),
                        ),
                    ),
                    ty.into(),
                ))
            },
        )
    }
}

struct ArrayIntoInner;

impl<'tcx> EvaluateExpr<'tcx> for ArrayIntoInner {
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

struct ArrayReverse;

impl<'tcx> EvaluateExpr<'tcx> for ArrayReverse {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;

        bit_vec_trans(
            generator,
            rec,
            ctx,
            |generator,
             ctx,
             BitVecTransArgs {
                 sig_ty,
                 rec,
                 bit_vec,
             }| {
                let ArrayDesc { count, width } = generator.array_desc(rec);

                Ok((
                    generator.net_list.add_node(
                        ctx.module_id,
                        BitVecTrans::new(
                            generator.net_list[bit_vec].ty.width(),
                            bit_vec,
                            generator.idents.for_module(ctx.module_id).tmp(),
                            move |buffer, input, output| {
                                buffer.write_template(format_args!(
                                    r#"
genvar i;
generate
for (i = 0; i < {count}; i = i + 1) begin
    assign {output}[({count} - 1 - i)*{width} +: {width}] = {input}[i*{width} +: {width}];
end
endgenerate"#
                                ));
                            },
                        ),
                    ),
                    sig_ty,
                ))
            },
        )
    }
}

struct Unbundle;

impl<'tcx> EvaluateExpr<'tcx> for Unbundle {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, args) = utils::expected_call(expr)?;

        generator.evaluate_expr(&args[0], ctx)
    }
}

struct Bundle;

impl<'tcx> EvaluateExpr<'tcx> for Bundle {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, args) = utils::expected_call(expr)?;

        generator.evaluate_expr(&args[0], ctx)
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
        from: SignalTy,
        target: SignalTy,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        if from == target {
            return generator.evaluate_expr(expr, ctx);
        }

        match (from, target) {
            (SignalTy::Prim(PrimTy::Bool), SignalTy::Prim(PrimTy::Bit)) => {
                assert_convert::<bool, Bit>();
                generator.evaluate_expr(expr, ctx)
            }
            (SignalTy::Prim(PrimTy::Bit), SignalTy::Prim(PrimTy::Bool)) => {
                assert_convert::<Bit, bool>();
                generator.evaluate_expr(expr, ctx)
            }
            (SignalTy::Prim(PrimTy::U128), SignalTy::Prim(PrimTy::Unsigned(n))) => {
                assert_convert::<u128, Unsigned<1>>();
                let node_id = generator.evaluate_expr(expr, ctx)?;

                let node_out = generator.net_list[node_id.node_id()]
                    .outputs_mut()
                    .only_one_mut()
                    .out;
                node_out.ty = PrimTy::Unsigned(n);

                Ok(node_id)
            }
            _ => {
                println!("from: {:?}", from);
                println!("to: {:?}", target);

                Err(
                    SpanError::new(SpanErrorKind::UnsupportedConversion, expr.span)
                        .into(),
                )
            }
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
                let from = generator.find_sig_ty(
                    &generator.node_type(args[0].hir_id),
                    ctx.generics,
                    args[0].span,
                )?;
                let target = match rec.kind {
                    ExprKind::Path(QPath::TypeRelative(ty, _)) => generator.find_sig_ty(
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
                let from = generator.find_sig_ty(
                    &generator.node_type(rec.hir_id),
                    ctx.generics,
                    rec.span,
                )?;
                let target = generator.find_sig_ty(
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
        PrimTy::U128 => evaluate_unsigned_lit(
            lit,
            prim_ty
                .width()
                .try_into()
                .map_err(|_| SpanError::new(SpanErrorKind::NotSynthExpr, lit.span))?,
        ),
        PrimTy::Unsigned(n) => evaluate_unsigned_lit(lit, n),
        PrimTy::BitVec(_) | PrimTy::Clock | PrimTy::ClockDomain => Err(SpanError::new(
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
