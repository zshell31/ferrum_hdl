pub mod array;
pub mod bin_op;
pub mod bitpack;
pub mod bitvec;
pub mod cast;
pub mod index;
pub mod signal;

use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::node::BinOp;
use rustc_hir::def_id::DefId;
use rustc_span::Span;

use crate::{
    compiler::{item::Item, item_ty::ItemTy, Compiler, Context},
    error::Error,
};

pub trait EvalExpr<'tcx> {
    fn eval(
        &self,
        compiler: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        output_ty: ItemTy<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error>;
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
            pub fn eval(
                &self,
                compiler: &mut Compiler<'tcx>,
                args: &[Item<'tcx>],
                output_ty: ItemTy<'tcx>,
                ctx: &mut Context<'tcx>,
                span: Span,
            ) -> Result<Item<'tcx>, Error> {
                match self.kind {
                    $(
                        BlackboxKind::$blackbox_kind => $eval.eval(compiler,  args, output_ty, ctx, span),
                    )+
                }
            }
        }
    };
}

macro_rules! args {
    ($args:ident as $( $arg:ident ),+) => {
        let [$($arg,)+ ..] = $args else { panic!("not enough arguments"); };
    };
}

use args;

struct PassReceiver;

impl<'tcx> EvalExpr<'tcx> for PassReceiver {
    fn eval(
        &self,
        _: &mut Compiler<'tcx>,
        args: &[Item<'tcx>],
        _: ItemTy<'tcx>,
        _: &mut Context<'tcx>,
        _: Span,
    ) -> Result<Item<'tcx>, Error> {
        args!(args as rec);

        Ok(rec.clone())
    }
}

eval_expr!(
    ArrayChain => array::Chain,

    BitPackPack => bitpack::BitPackPack,
    BitPackUnpack => bitpack::BitPackUnpack,
    BitPackMsb => bitpack::BitPackMsb,

    BitVecSlice => bitvec::BitVecSlice,

    Bundle => PassReceiver,
    Unbundle => PassReceiver,

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

    CastFrom => cast::Conversion,

    Index => index::Index,

    SignalAndThen => signal::SignalAndThen,
    SignalApply2 => signal::SignalApply2,
    SignalLift => PassReceiver,
    SignalMap => signal::SignalMap,
    SignalReg => signal::SignalReg { has_en: false },
    SignalRegEn => signal::SignalReg { has_en: true },
    SignalValue => PassReceiver,
    SignalWatch => PassReceiver,

    StdClone => PassReceiver
);
