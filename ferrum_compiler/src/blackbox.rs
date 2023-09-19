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
    Cast,
    BitL,
    BitH,
    RegisterFn,
    RegisterFnEn,
    SignalLift,
    SignalMap,
    SignalAndThen,
    SignalApply2,
    SignalValue,
    BitPackPack,
    BitPackMsb,
    BitVecShrink,
    BitVecSlice,
    BitVecUnpack,
    ArrayIntoInner,
    ArrayReverse,
    ArrayMap,
    Unbundle,
    Bundle,
    StdConversion { from: bool },
    StdClone,
}

pub fn find_blackbox(def_path: &DefPath) -> Option<Blackbox> {
    // TODO: check crate
    if def_path == &ItemPath(&["Cast", "cast"]) {
        return Some(Blackbox::Cast);
    }

    if def_path == &ItemPath(&["bit", "L"]) {
        return Some(Blackbox::BitL);
    }

    if def_path == &ItemPath(&["bit", "H"]) {
        return Some(Blackbox::BitH);
    }

    if def_path == &ItemPath(&["signal", "reg"]) {
        return Some(Blackbox::RegisterFn);
    }

    if def_path == &ItemPath(&["signal", "reg_en"]) {
        return Some(Blackbox::RegisterFnEn);
    }

    if def_path == &ItemPath(&["signal", "impl", "lift"]) {
        return Some(Blackbox::SignalLift);
    }

    if def_path == &ItemPath(&["signal", "impl", "map"]) {
        return Some(Blackbox::SignalMap);
    }

    if def_path == &ItemPath(&["signal", "impl", "and_then"]) {
        return Some(Blackbox::SignalAndThen);
    }

    if def_path == &ItemPath(&["signal", "impl", "value"]) {
        return Some(Blackbox::SignalValue);
    }

    if def_path == &ItemPath(&["signal", "apply2"]) {
        return Some(Blackbox::SignalApply2);
    }

    if def_path == &ItemPath(&["bit_pack", "BitPack", "pack"]) {
        return Some(Blackbox::BitPackPack);
    }

    if def_path == &ItemPath(&["bit_pack", "BitPack", "msb"]) {
        return Some(Blackbox::BitPackMsb);
    }

    if def_path == &ItemPath(&["bit_vec", "impl", "shrink"]) {
        return Some(Blackbox::BitVecShrink);
    }

    if def_path == &ItemPath(&["bit_vec", "impl", "slice"]) {
        return Some(Blackbox::BitVecSlice);
    }

    if def_path == &ItemPath(&["bit_vec", "impl", "unpack"]) {
        return Some(Blackbox::BitVecUnpack);
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
        return Some(Blackbox::StdConversion { from: true });
    }

    if def_path == &ItemPath(&["convert", "Into", "into"]) {
        return Some(Blackbox::StdConversion { from: false });
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
        return key.generic_ty(1);
    }

    if def_path == &ItemPath(&["signal", "Wrapped"]) {
        return key.generic_ty(1);
    }

    if def_path == &ItemPath(&["bit_vec", "BitVec"]) {
        return key.generic_const(0).map(|val| PrimTy::BitVec(val).into());
    }

    if def_path == &ItemPath(&["bit", "Bit"]) {
        return Some(PrimTy::Bit.into());
    }

    if def_path == &ItemPath(&["domain", "Clock"]) {
        return Some(PrimTy::Clock.into());
    }

    if def_path == &ItemPath(&["unsigned", "Unsigned"]) {
        return key.generic_const(0).map(|val| PrimTy::Unsigned(val).into());
    }

    if def_path == &ItemPath(&["array", "Array"]) {
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
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error>;
}

impl<'tcx> EvaluateExpr<'tcx> for Blackbox {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        match self {
            Self::Cast => Cast.evaluate_expr(generator, expr, ctx),
            Self::BitL => BitVal(false).evaluate_expr(generator, expr, ctx),
            Self::BitH => BitVal(true).evaluate_expr(generator, expr, ctx),
            Self::RegisterFn => {
                RegisterFn { has_en: false }.evaluate_expr(generator, expr, ctx)
            }
            Self::RegisterFnEn => {
                RegisterFn { has_en: true }.evaluate_expr(generator, expr, ctx)
            }
            Self::SignalLift => SignalLift.evaluate_expr(generator, expr, ctx),
            Self::SignalMap => SignalMap.evaluate_expr(generator, expr, ctx),
            Self::SignalAndThen => SignalAndThen.evaluate_expr(generator, expr, ctx),
            Self::SignalApply2 => SignalApply2.evaluate_expr(generator, expr, ctx),
            Self::SignalValue => SignalValue.evaluate_expr(generator, expr, ctx),
            Self::BitPackPack => BitPackPack.evaluate_expr(generator, expr, ctx),
            Self::BitPackMsb => BitPackMsb.evaluate_expr(generator, expr, ctx),
            Self::BitVecShrink => BitVecShrink.evaluate_expr(generator, expr, ctx),
            Self::BitVecSlice => BitVecSlice.evaluate_expr(generator, expr, ctx),
            Self::BitVecUnpack => BitVecUnpack.evaluate_expr(generator, expr, ctx),
            Self::ArrayIntoInner => ArrayIntoInner.evaluate_expr(generator, expr, ctx),
            Self::ArrayReverse => ArrayReverse.evaluate_expr(generator, expr, ctx),
            Self::ArrayMap => ArrayMap.evaluate_expr(generator, expr, ctx),
            Self::Unbundle => Unbundle.evaluate_expr(generator, expr, ctx),
            Self::Bundle => Bundle.evaluate_expr(generator, expr, ctx),
            Self::StdConversion { from } => {
                StdConversion { from: *from }.evaluate_expr(generator, expr, ctx)
            }
            Self::StdClone => StdClone.evaluate_expr(generator, expr, ctx),
        }
    }
}

struct Cast;

impl<'tcx> EvaluateExpr<'tcx> for Cast {
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

struct BitVal(bool);

impl BitVal {
    fn create_bit_value<'tcx>(
        &self,
        generator: &mut Generator<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let value = match self.0 {
            true => 1,
            false => 0,
        };

        let cons = ConstNode::new(
            PrimTy::Bit,
            value,
            generator.idents.for_module(ctx.module_id).tmp(),
        );
        Ok(generator.net_list.add_node(ctx.module_id, cons).into())
    }
}

impl<'tcx> EvaluateExpr<'tcx> for BitVal {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        _: &Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        self.create_bit_value(generator, ctx)
    }
}

struct RegisterFn {
    has_en: bool,
}

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
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (rec, args) = utils::expected_call(expr)?;

        let ty = generator.node_type(rec.hir_id, ctx);

        let value_ty =
            utils::subst_type(ty, 1).ok_or_else(|| Self::make_err(rec.span))?;
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

        // TODO: refactor truncating
        let len = generator.net_list.module_len(ctx.module_id);
        let rst_val_span = rst_val.span;
        let rst_val = generator.evaluate_expr(rst_val, ctx)?;
        let rst_val = generator
            .to_const(rst_val)
            .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedConst, rst_val_span))?;
        generator.net_list.module_truncate(ctx.module_id, len);

        let comb = generator.evaluate_expr(comb, ctx)?;

        let comb_out = generator.maybe_to_bitvec(ctx.module_id, comb);

        let prim_ty = sig_ty.maybe_to_bitvec();

        let dff = generator.net_list.add_node(
            ctx.module_id,
            DFFNode::new(
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

struct SignalLift;

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

struct SignalMap;

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

struct SignalAndThen;

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

struct SignalApply2;

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

struct SignalValue;

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

pub fn bit_vec_trans<'tcx>(
    generator: &mut Generator<'tcx>,
    source: ItemId,
    ctx: &EvalContext<'tcx>,
    trans: impl FnOnce(
        &mut Generator<'tcx>,
        &EvalContext<'tcx>,
        NodeOutId,
    ) -> Result<(NodeId, SignalTy), Error>,
) -> Result<ItemId, Error> {
    let bit_vec = generator.to_bitvec(ctx.module_id, source);

    let (trans, sig_ty) = trans(generator, ctx, bit_vec)?;
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
    ctx: &EvalContext<'tcx>,
    count: u128,
    trans: impl FnOnce(
        &mut Generator<'tcx>,
        &EvalContext<'tcx>,
        LoopArgs,
    ) -> Result<SignalTy, Error>,
) -> Result<ItemId, Error> {
    bit_vec_trans(generator, source, ctx, |generator, ctx, bit_vec| {
        let loop_var = generator.idents.for_module(ctx.module_id).ident("i");
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

struct BitPackPack;

impl<'tcx> EvaluateExpr<'tcx> for BitPackPack {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;
        let rec = generator.evaluate_expr(rec, ctx)?;

        Ok(generator.to_bitvec(ctx.module_id, rec).node_id().into())
    }
}

struct BitPackMsb;

impl<'tcx> EvaluateExpr<'tcx> for BitPackMsb {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
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

struct BitVecShrink;

impl<'tcx> EvaluateExpr<'tcx> for BitVecShrink {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;
        let rec = generator.evaluate_expr(rec, ctx)?;

        let ty = generator.node_type(expr.hir_id, ctx);
        let width = generator
            .find_sig_ty(ty, ctx.generic_args, expr.span)?
            .width();

        let rec = generator.maybe_to_bitvec(ctx.module_id, rec);

        Ok(generator
            .net_list
            .add_node(
                ctx.module_id,
                Splitter::new(
                    rec,
                    [(
                        PrimTy::BitVec(width),
                        generator.idents.for_module(ctx.module_id).tmp(),
                    )],
                    None,
                ),
            )
            .into())
    }
}

struct BitVecSlice;

impl<'tcx> EvaluateExpr<'tcx> for BitVecSlice {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;
        let rec = generator.evaluate_expr(rec, ctx)?.node_id();
        let rec = generator.net_list[rec]
            .outputs()
            .only_one()
            .node_out_id(rec);

        let generics = generator.method_call_generics(expr, ctx)?;

        let start = generics.as_const(1).unwrap();
        let width = generics.as_const(2).unwrap();

        Ok(generator
            .net_list
            .add_node(
                ctx.module_id,
                Splitter::new(
                    rec,
                    [(
                        PrimTy::BitVec(width),
                        generator.idents.for_module(ctx.module_id).tmp(),
                    )],
                    Some(start),
                ),
            )
            .into())
    }
}

struct BitVecUnpack;

impl<'tcx> EvaluateExpr<'tcx> for BitVecUnpack {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;
        let rec = generator.evaluate_expr(rec, ctx)?.node_id();
        let rec = generator.net_list[rec]
            .outputs()
            .only_one()
            .node_out_id(rec);

        let ty = generator.node_type(expr.hir_id, ctx);
        let sig_ty = generator.find_sig_ty(ty, ctx.generic_args, expr.span)?;

        Ok(generator.from_bitvec(ctx.module_id, rec, sig_ty))
    }
}

struct ArrayIntoInner;

impl<'tcx> EvaluateExpr<'tcx> for ArrayIntoInner {
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

struct ArrayReverse;

impl<'tcx> EvaluateExpr<'tcx> for ArrayReverse {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, _, _) = utils::exptected_method_call(expr)?;

        let ArrayTy(count, sig_ty) = generator
            .find_sig_ty(
                generator.node_type(rec.hir_id, ctx),
                ctx.generic_args,
                rec.span,
            )?
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
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, rec, args, _) = utils::exptected_method_call(expr)?;
        let span = rec.span;

        let ArrayTy(count, sig_ty) = generator
            .find_sig_ty(
                generator.node_type(rec.hir_id, ctx),
                ctx.generic_args,
                rec.span,
            )?
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
                let input = generator.net_list[input]
                    .outputs()
                    .only_one()
                    .node_out_id(input);
                let input = generator.from_bitvec(ctx.module_id, input, *sig_ty);

                let closure = generator.evaluate_expr(closure, ctx)?;
                generator.link_dummy_inputs(&[input], closure, span)?;

                let sig_ty = generator.item_ty(closure);
                let width = sig_ty.width();

                let closure_out = generator.maybe_to_bitvec(ctx.module_id, closure);
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
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
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
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let (_, args) = utils::expected_call(expr)?;

        generator.evaluate_expr(&args[0], ctx)
    }
}

#[allow(dead_code)]
pub struct StdConversion {
    from: bool,
}

impl StdConversion {
    fn make_err(span: Span) -> Error {
        SpanError::new(
            SpanErrorKind::NotSynthBlackboxExpr(Blackbox::RegisterFn),
            span,
        )
        .into()
    }

    pub fn convert(
        from: SignalTy,
        target: SignalTy,
        generator: &mut Generator<'_>,
        item_id: ItemId,
        span: Span,
    ) -> Result<ItemId, Error> {
        if from == target {
            return Ok(item_id);
        }

        let node_id = item_id.node_id();
        let module_id = node_id.module_id();
        let node_out = generator.net_list[node_id].outputs_mut().only_one_mut();
        let node_out_id = node_out.node_out_id(node_id);
        let node_out = node_out.out;

        match (from, target) {
            (SignalTy::Prim(PrimTy::Bool), SignalTy::Prim(PrimTy::Bit)) => {
                assert_convert::<bool, Bit>();
                node_out.ty = PrimTy::Bit;
                Ok(item_id)
            }
            (SignalTy::Prim(PrimTy::Bit), SignalTy::Prim(PrimTy::Bool)) => {
                assert_convert::<Bit, bool>();
                node_out.ty = PrimTy::Bool;
                Ok(item_id)
            }
            (SignalTy::Prim(PrimTy::U128), SignalTy::Prim(PrimTy::Unsigned(n))) => {
                assert_convert::<u128, Unsigned<1>>();
                Ok(generator
                    .net_list
                    .add_node(
                        module_id,
                        Splitter::new(
                            node_out_id,
                            [(
                                PrimTy::Unsigned(n),
                                generator.idents.for_module(module_id).tmp(),
                            )],
                            None,
                        ),
                    )
                    .into())
            }
            _ => {
                println!("from: {:?}", from);
                println!("to: {:?}", target);

                Err(SpanError::new(SpanErrorKind::UnsupportedConversion, span).into())
            }
        }
    }

    fn convert_for_expr<'tcx>(
        from: SignalTy,
        target: SignalTy,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        let item_id = generator.evaluate_expr(expr, ctx)?;
        Self::convert(from, target, generator, item_id, expr.span)
    }
}

fn assert_convert<F, T: From<F>>() {}

impl<'tcx> EvaluateExpr<'tcx> for StdConversion {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error> {
        match expr.kind {
            ExprKind::Call(rec, args) => {
                let from = generator.find_sig_ty(
                    generator.node_type(args[0].hir_id, ctx),
                    ctx.generic_args,
                    args[0].span,
                )?;
                let target = match rec.kind {
                    ExprKind::Path(QPath::TypeRelative(ty, _)) => generator.find_sig_ty(
                        generator.node_type(ty.hir_id, ctx),
                        ctx.generic_args,
                        rec.span,
                    )?,
                    _ => {
                        return Err(Self::make_err(rec.span));
                    }
                };

                Self::convert_for_expr(from, target, generator, &args[0], ctx)
            }
            ExprKind::MethodCall(_, rec, _, span) => {
                let from = generator.find_sig_ty(
                    generator.node_type(rec.hir_id, ctx),
                    ctx.generic_args,
                    rec.span,
                )?;
                let target = generator.find_sig_ty(
                    generator.node_type(expr.hir_id, ctx),
                    ctx.generic_args,
                    span,
                )?;

                Self::convert_for_expr(from, target, generator, rec, ctx)
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
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
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

fn evaluate_unsigned_lit(lit: &Lit, width: u128) -> Result<u128, Error> {
    match lit.node {
        LitKind::Int(n, _) => Ok(unsigned_value(n, width)),
        _ => Err(SpanError::new(
            SpanErrorKind::UnexpectedLitValue(PrimTy::Unsigned(width)),
            lit.span,
        )
        .into()),
    }
}
