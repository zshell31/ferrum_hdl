use ferrum::{
    bit::{bit_value, Bit},
    unsigned::{unsigned_value, Unsigned},
};
use ferrum_netlist::{
    group_list::ItemId,
    net_list::{NodeId, NodeOutId},
    node::{
        ConstNode, DFFNode, Expr as ExprNode, IsNode, LoopEnd, LoopStart, Node, Splitter,
    },
    params::Outputs,
    sig_ty::{ArrayTy, PrimTy, SignalTy},
    symbol::Symbol,
};
use rustc_ast::LitKind;
use rustc_hir::{
    definitions::{DefPath, DefPathDataName},
    Expr, ExprKind, Lit, QPath,
};
use rustc_span::Span;

use crate::{
    error::{Error, SpanError, SpanErrorKind},
    generator::{ty_or_def_id::TyOrDefIdWithGen, EvalContext, Generator},
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Blackbox {
    BitL,
    BitH,
    RegisterFn,
    SignalLift,
    SignalMap,
    SignalApply2,
    BitPackMsb,
    ArrayIntoInner,
    ArrayReverse,
    ArrayMap,
    Unbundle,
    Bundle,
    StdConversion,
    StdClone,
}

pub fn find_blackbox(def_path: &DefPath) -> Option<Blackbox> {
    // TODO: check crate
    if def_path == &ItemPath(&["bit", "L"]) {
        return Some(Blackbox::BitL);
    }

    if def_path == &ItemPath(&["bit", "H"]) {
        return Some(Blackbox::BitH);
    }

    if def_path == &ItemPath(&["signal", "reg"]) {
        return Some(Blackbox::RegisterFn);
    }

    if def_path == &ItemPath(&["signal", "impl", "lift"]) {
        return Some(Blackbox::SignalLift);
    }

    if def_path == &ItemPath(&["signal", "impl", "map"]) {
        return Some(Blackbox::SignalMap);
    }

    if def_path == &ItemPath(&["signal", "apply2"]) {
        return Some(Blackbox::SignalApply2);
    }

    if def_path == &ItemPath(&["bit_pack", "BitPack", "msb"]) {
        return Some(Blackbox::BitPackMsb);
    }

    if def_path == &ItemPath(&["array", "impl", "into_inner"]) {
        return Some(Blackbox::ArrayIntoInner);
    }

    if def_path == &ItemPath(&["array", "impl", "reverse"]) {
        return Some(Blackbox::ArrayReverse);
    }

    if def_path == &ItemPath(&["array", "impl", "map"]) {
        return Some(Blackbox::ArrayMap);
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

pub fn ignore_ty(def_path: &DefPath) -> bool {
    if def_path == &ItemPath(&["marker", "PhantomData"]) {
        return true;
    }

    false
}

pub fn find_sig_ty(key: &TyOrDefIdWithGen<'_>, def_path: &DefPath) -> Option<SignalTy> {
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

        return Some(SignalTy::mk_array(n, ty));
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
            Self::BitL => BitL.evaluate_expr(generator, expr, ctx),
            Self::BitH => BitH.evaluate_expr(generator, expr, ctx),
            Self::RegisterFn => RegisterFn.evaluate_expr(generator, expr, ctx),
            Self::SignalLift => SignalLift.evaluate_expr(generator, expr, ctx),
            Self::SignalMap => SignalMap.evaluate_expr(generator, expr, ctx),
            Self::SignalApply2 => SignalApply2.evaluate_expr(generator, expr, ctx),
            Self::BitPackMsb => BitPackMsb.evaluate_expr(generator, expr, ctx),
            Self::ArrayIntoInner => ArrayIntoInner.evaluate_expr(generator, expr, ctx),
            Self::ArrayReverse => ArrayReverse.evaluate_expr(generator, expr, ctx),
            Self::ArrayMap => ArrayMap.evaluate_expr(generator, expr, ctx),
            Self::Unbundle => Unbundle.evaluate_expr(generator, expr, ctx),
            Self::Bundle => Bundle.evaluate_expr(generator, expr, ctx),
            Self::StdConversion => StdConversion.evaluate_expr(generator, expr, ctx),
            Self::StdClone => StdClone.evaluate_expr(generator, expr, ctx),
        }
    }
}

fn create_bit_value<'tcx>(
    generator: &mut Generator<'tcx>,
    value: u128,
    ctx: EvalContext<'tcx>,
) -> Result<ItemId, Error> {
    let cons = ConstNode::new(
        PrimTy::Bit,
        value,
        generator.idents.for_module(ctx.module_id).tmp(),
    );
    Ok(generator.net_list.add_node(ctx.module_id, cons).into())
}

struct BitL;

impl<'tcx> EvaluateExpr<'tcx> for BitL {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        _: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        create_bit_value(generator, 0, ctx)
    }
}

struct BitH;

impl<'tcx> EvaluateExpr<'tcx> for BitH {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        _: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        create_bit_value(generator, 1, ctx)
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

        let signal_val_ty = utils::subst_type(ty, 1).and_then(|ty| {
            generator
                .find_sig_ty(ty, ctx.generic_args, rec.span)
                .ok()
                .map(|sig_ty| sig_ty.prim_ty())
        });

        let gen = utils::subst_type(ty, 1).ok_or_else(|| Self::make_err(rec.span))?;
        let prim_ty = generator
            .find_sig_ty(gen, ctx.generic_args, rec.span)?
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

struct SignalLift;

impl<'tcx> EvaluateExpr<'tcx> for SignalLift {
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

pub fn bit_vec_trans<'tcx>(
    generator: &mut Generator<'tcx>,
    source: ItemId,
    ctx: EvalContext<'tcx>,
    trans: impl FnOnce(
        &mut Generator<'tcx>,
        &EvalContext<'tcx>,
        NodeOutId,
    ) -> Result<(NodeId, SignalTy), Error>,
) -> Result<ItemId, Error> {
    let bit_vec = generator.to_bitvec(ctx.module_id, source);

    let (trans, sig_ty) = trans(generator, &ctx, bit_vec)?;
    let trans = generator.net_list[trans]
        .outputs()
        .only_one()
        .node_out_id(trans);

    let from = generator.from_bitvec(ctx.module_id, trans, sig_ty);

    Ok(from)
}

pub struct LoopArgs {
    input: NodeOutId,
    output: Symbol,
    loop_var: Symbol,
}

pub fn bit_vec_trans_in_loop<'tcx>(
    generator: &mut Generator<'tcx>,
    source: ItemId,
    ctx: EvalContext<'tcx>,
    count: u128,
    trans: impl FnOnce(
        &mut Generator<'tcx>,
        &EvalContext<'tcx>,
        LoopArgs,
    ) -> Result<SignalTy, Error>,
) -> Result<ItemId, Error> {
    bit_vec_trans(generator, source, ctx, |generator, ctx, bit_vec| {
        let loop_var = Symbol::new("i");
        let output = generator.idents.for_module(ctx.module_id).tmp();

        let loop_id = generator
            .net_list
            .add_node(ctx.module_id, LoopStart::new(loop_var, count, None));

        let sig_ty = trans(generator, ctx, LoopArgs {
            input: bit_vec,
            output,
            loop_var,
        })?;
        let width = sig_ty.width();

        generator.net_list.add_node(ctx.module_id, LoopEnd {});

        if let Node::LoopStart(node) = &mut generator.net_list[loop_id] {
            node.set_out(Some((PrimTy::BitVec(count * width), output)));
        }

        Ok((loop_id, SignalTy::mk_array(count, sig_ty)))
    })
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
        let rec = generator.evaluate_expr(rec, ctx)?;

        bit_vec_trans(generator, rec, ctx, |generator, ctx, bit_vec| {
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
        })
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

        let ArrayTy(count, sig_ty) = generator
            .find_sig_ty(generator.node_type(rec.hir_id), ctx.generic_args, rec.span)?
            .array_ty();
        let width = sig_ty.width();

        let rec = generator.evaluate_expr(rec, ctx)?;

        bit_vec_trans_in_loop(
            generator,
            rec,
            ctx,
            count,
            |generator,
             ctx,
             LoopArgs {
                 input,
                 output,
                 loop_var,
             }| {
                let bitvec_ty = generator.net_list[input].ty;

                generator.net_list.add_node(ctx.module_id, ExprNode::new(bitvec_ty, input, output, true, move |buffer, input, output| {
                    buffer.write_template(format_args!("assign {output}[{count} - 1 - {loop_var}*{width} +: {width}] = {input}[{loop_var}*{width} +: {width}];"));
                }));

                Ok(*sig_ty)
            },
        )
    }
}

struct ArrayMap;

impl<'tcx> EvaluateExpr<'tcx> for ArrayMap {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, args, _) = utils::exptected_method_call(expr)?;
        let span = rec.span;

        let ArrayTy(count, sig_ty) = generator
            .find_sig_ty(generator.node_type(rec.hir_id), ctx.generic_args, rec.span)?
            .array_ty();
        let width = sig_ty.width();

        let rec = generator.evaluate_expr(rec, ctx)?;

        let closure = &args[0];
        bit_vec_trans_in_loop(
            generator,
            rec,
            ctx,
            count,
            move |generator,
                  ctx,
                  LoopArgs {
                      input,
                      output,
                      loop_var,
                  }| {
                let map_in = generator.idents.for_module(ctx.module_id).ident("map_in");

                let input = generator.net_list.add_node(ctx.module_id, ExprNode::new(PrimTy::BitVec(width), input, map_in, false, move |buffer, input, output| {
                    buffer.write_template(format_args!("assign {output} = {input}[{loop_var}*{width} +: {width}];"));
                }));

                let closure = generator.evaluate_expr(closure, *ctx)?;
                generator.link_dummy_inputs(&[input.into()], closure, span)?;

                let sig_ty = generator.item_ty(closure);
                let width = sig_ty.width();

                let closure_out = match closure {
                    ItemId::Node(node_id) => {
                        let out = generator.net_list[node_id].outputs();
                        if out.len() == 1 {
                            out.only_one().node_out_id(node_id)
                        } else {
                            generator.to_bitvec(ctx.module_id, closure)
                        }
                    }
                    ItemId::Group(_) => generator.to_bitvec(ctx.module_id, closure),
                };

                let map_out = generator.idents.for_module(ctx.module_id).ident("map_out");
                generator.net_list[closure_out].sym = map_out;

                generator.net_list.add_node(ctx.module_id, ExprNode::new(PrimTy::BitVec(width), closure_out, output, true, move |buffer, input, output| {
                    buffer.write_template(format_args!("assign {output}[{loop_var}*{width} +: {width}] = {input};"));
                }));

                Ok(sig_ty)
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
                    generator.node_type(args[0].hir_id),
                    ctx.generic_args,
                    args[0].span,
                )?;
                let target = match rec.kind {
                    ExprKind::Path(QPath::TypeRelative(ty, _)) => generator.find_sig_ty(
                        generator.node_type(ty.hir_id),
                        ctx.generic_args,
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
                    generator.node_type(rec.hir_id),
                    ctx.generic_args,
                    rec.span,
                )?;
                let target = generator.find_sig_ty(
                    generator.node_type(expr.hir_id),
                    ctx.generic_args,
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
