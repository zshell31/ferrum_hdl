pub mod array;
pub mod bin_op;
pub mod bit;
pub mod bitpack;
pub mod bitvec;
pub mod bundle;
pub mod cast;
pub mod index;
pub mod lit;
pub mod signal;
pub mod std;

use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{group::ItemId, node::BinOp, sig_ty::SignalTy};
use rustc_hir::{def_id::DefId, Expr};
use rustc_span::Span;

use crate::{
    error::Error,
    eval_context::{EvalContext, ModuleOrItem},
    generator::Generator,
};

pub trait EvalExpr<'tcx> {
    fn eval_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error>;

    fn eval(
        &self,
        generator: &mut Generator<'tcx>,
        args: &[ModuleOrItem],
        output_ty: SignalTy,
        ctx: &mut EvalContext<'tcx>,
        span: Span,
    ) -> Result<ItemId, Error> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct Blackbox {
    pub kind: BlackboxKind,
    pub fn_did: DefId,
}

macro_rules! eval_expr {
    (
        $( $blackbox_kind:ident => $eval:expr ),+
    ) => {
        impl<'tcx> Blackbox {
            pub fn eval_expr(
                &self,
                generator: &mut Generator<'tcx>,
                expr: &'tcx Expr<'tcx>,
                ctx: &mut EvalContext<'tcx>
            ) -> Result<ItemId, Error> {
                match self.kind {
                    $(
                        BlackboxKind::$blackbox_kind => $eval.eval_expr(generator, expr, ctx),
                    )+
                }
            }

            pub fn eval(
                &self,
                generator: &mut Generator<'tcx>,
                args: &[ModuleOrItem],
                output_ty: SignalTy,
                ctx: &mut EvalContext<'tcx>,
                span: Span,
            ) -> Result<ItemId, Error> {
                match self.kind {
                    $(
                        BlackboxKind::$blackbox_kind => $eval.eval(generator,  args, output_ty, ctx, span),
                    )+
                }
            }
        }
    };
}

eval_expr!(
    ArrayReverse => array::Reverse,
    ArrayMap => array::Map,
    ArrayMake => array::Make,


    BitL => bit::BitVal(false),
    BitH => bit::BitVal(true),

    BitPackPack => bitpack::BitPackPack,
    BitPackRepack => bitpack::BitPackRepack,
    BitPackMsb => bitpack::BitPackMsb,

    BitVecShrink => bitvec::BitVecShrink,
    BitVecSlice => bitvec::BitVecSlice,
    BitVecUnpack => bitvec::BitVecUnpack,

    Bundle => bundle::Bundle,
    Unbundle => bundle::Unbundle,

    OpEq => bin_op::BinOp(BinOp::Eq),
    OpNe => bin_op::BinOp(BinOp::Ne),
    OpLt => bin_op::BinOp(BinOp::Lt),
    OpLe => bin_op::BinOp(BinOp::Le),
    OpGt => bin_op::BinOp(BinOp::Gt),
    OpGe => bin_op::BinOp(BinOp::Ge),
    OpBitAnd => bin_op::BinOp(BinOp::BitAnd),
    OpBitOr => bin_op::BinOp(BinOp::BitOr),
    OpBitXor => bin_op::BinOp(BinOp::BitXor),
    OpAnd => bin_op::BinOp(BinOp::And),
    OpOr => bin_op::BinOp(BinOp::Or),
    OpAdd => bin_op::BinOp(BinOp::Add),
    OpSub => bin_op::BinOp(BinOp::Sub),
    OpMul => bin_op::BinOp(BinOp::Mul),
    OpDiv => bin_op::BinOp(BinOp::Div),
    OpRem => bin_op::BinOp(BinOp::Rem),
    OpShl => bin_op::BinOp(BinOp::Shl),
    OpShr => bin_op::BinOp(BinOp::Shr),

    CastFrom => cast::Conversion { from: true },
    Cast => cast::Conversion { from: true },

    Index => index::Index,

    SignalAndThen => signal::SignalAndThen,
    SignalApply2 => signal::SignalApply2,
    SignalAnd => signal::SignalOp { op: BinOp::And },
    SignalEq => signal::SignalOp { op: BinOp::Eq },
    SignalLift => signal::SignalLift,
    SignalMap => signal::SignalMap,
    SignalOr => signal::SignalOp { op: BinOp::Or },
    SignalReg => signal::SignalReg { has_en: false },
    SignalRegEn => signal::SignalReg { has_en: true },
    SignalValue => signal::SignalValue,
    SignalWatch => signal::SignalWatch,

    UnsignedBit => index::UnsignedBit,

    StdClone => std::StdClone

);
