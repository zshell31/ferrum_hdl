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

use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{group::ItemId, node::BinOp};
use rustc_hir::Expr;

use crate::{
    error::Error,
    eval_context::EvalContext,
    generator::{ty_or_def_id::TyOrDefIdWithGen, Generator},
};

pub trait EvalExpr<'tcx> {
    fn eval_expr(
        &self,
        blackbox: &Blackbox<'tcx>,
        generator: &mut Generator<'tcx>,
        expr: &'tcx Expr<'tcx>,
        ctx: &mut EvalContext<'tcx>,
    ) -> Result<ItemId, Error>;
}

#[derive(Debug)]
pub struct Blackbox<'tcx> {
    pub kind: BlackboxKind,
    pub key: TyOrDefIdWithGen<'tcx>,
}

macro_rules! eval_expr {
    (
        $( $blackbox_kind:ident => $eval:expr ),+
    ) => {
        impl<'tcx> Blackbox<'tcx> {
            pub fn eval_expr(
                &self,
                generator: &mut Generator<'tcx>,
                expr: &'tcx Expr<'tcx>,
                ctx: &mut EvalContext<'tcx>
            ) -> Result<ItemId, Error> {
                match self.kind {
                    $(
                        BlackboxKind::$blackbox_kind => $eval.eval_expr(self, generator, expr, ctx),
                    )+
                }
            }
        }
    };
}

eval_expr!(
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

    CastFrom => cast::Conversion { from: true },
    Cast => cast::Conversion { from: true },

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

    IdxDefault => index::IdxDefault,
    IdxVal => index::IdxVal,
    IdxSucc => index::IdxSucc,
    IdxPred => index::IdxPred,
    IdxIsMin => index::IdxIsMin,
    IdxIsMax => index::IdxIsMax,

    StdClone => std::StdClone

);
