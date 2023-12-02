pub mod array;
pub mod bit;
pub mod bitpack;
pub mod bitvec;
pub mod bundle;
pub mod cast;
pub mod index;
pub mod lit;
pub mod signal;
pub mod std;

use fhdl_blackbox::Blackbox;
use fhdl_netlist::{group::ItemId, node::BinOp};
use rustc_hir::Expr;

use crate::{
    error::Error,
    generator::{EvalContext, Generator},
};

pub trait EvaluateExpr<'tcx> {
    fn evaluate_expr(
        &self,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &EvalContext<'tcx>,
    ) -> Result<ItemId, Error>;
}

macro_rules! evaluate_expr {
    (
        $( $blackbox_kind:ident => $eval:expr ),+
    ) => {
        impl<'tcx> EvaluateExpr<'tcx> for Blackbox {
            fn evaluate_expr(
                &self,
                generator: &mut Generator<'tcx>,
                expr: &'tcx Expr<'tcx>,
                ctx: &EvalContext<'tcx>
            ) -> Result<ItemId, Error> {
                match self {
                    $(
                        Self::$blackbox_kind => $eval.evaluate_expr(generator, expr, ctx),
                    )+
                }
            }
        }
    };
}

evaluate_expr!(
    ArrayReverse => array::ArrayReverse,
    ArrayMap => array::ArrayMap,

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

    Cast => cast::Cast,
    StdFrom => cast::StdConversion { from: true },
    StdInto => cast::StdConversion { from: false },

    SignalAndThen => signal::SignalAndThen,
    SignalApply2 => signal::SignalApply2,
    SignalAnd => signal::SignalOp { op: BinOp::And },
    SignalEq => signal::SignalOp { op: BinOp::Eq },
    SignalLift => signal::SignalLift,
    SignalMap => signal::SignalMap,
    SignalOr => signal::SignalOp { op: BinOp::Or },
    SignalReg => signal::SignalReg { has_en: false },
    SignalRegEn => signal::SignalReg { has_en: true },
    SignalReset => bit::BitVal(false),
    SignalValue => signal::SignalValue,
    SignalWatch => signal::SignalWatch,

    UnsignedIndex => index::UnsignedIndex,

    StdClone => std::StdClone

);
