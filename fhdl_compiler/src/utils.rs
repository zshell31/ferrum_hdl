use std::iter;

use fhdl_netlist::node::BinOp;
use rustc_const_eval::interpret::Scalar;
use rustc_hir::{def_id::DefId, BinOpKind, Expr, ExprKind, HirId, Pat};
use rustc_middle::{
    mir::ConstValue,
    ty::{GenericArgsRef, ScalarInt, Ty, TyKind, ValTree},
};
use rustc_span::symbol::Ident;
use smallvec::SmallVec;

use crate::error::{Error, SpanError, SpanErrorKind};

pub fn to_bin_op(kind: BinOpKind) -> BinOp {
    match kind {
        BinOpKind::Add => BinOp::Add,
        BinOpKind::And => BinOp::And,
        BinOpKind::BitAnd => BinOp::BitAnd,
        BinOpKind::BitOr => BinOp::BitOr,
        BinOpKind::BitXor => BinOp::BitXor,
        BinOpKind::Div => BinOp::Div,
        BinOpKind::Eq => BinOp::Eq,
        BinOpKind::Ge => BinOp::Ge,
        BinOpKind::Gt => BinOp::Gt,
        BinOpKind::Le => BinOp::Le,
        BinOpKind::Lt => BinOp::Lt,
        BinOpKind::Mul => BinOp::Mul,
        BinOpKind::Ne => BinOp::Ne,
        BinOpKind::Or => BinOp::Or,
        BinOpKind::Rem => BinOp::Rem,
        BinOpKind::Shl => BinOp::Shl,
        BinOpKind::Shr => BinOp::Shr,
        BinOpKind::Sub => BinOp::Sub,
    }
}

pub fn ty_def_id(ty: Ty<'_>) -> Option<DefId> {
    match ty.kind() {
        TyKind::Adt(adt, _) => Some(adt.did()),
        TyKind::FnDef(did, _) => Some(*did),
        _ => None,
    }
}

pub fn subst(ty: Ty<'_>) -> Option<GenericArgsRef<'_>> {
    match ty.kind() {
        TyKind::Adt(_, subst) => Some(subst),
        TyKind::FnDef(_, subst) => Some(subst),
        _ => None,
    }
}

pub fn subst_type(ty: Ty<'_>, index: usize) -> Option<Ty<'_>> {
    subst(ty).and_then(|subst| subst.get(index).and_then(|generic| generic.as_type()))
}

pub struct CallArgs<'tcx> {
    pub fn_item: HirId,
    pub args: SmallVec<[&'tcx Expr<'tcx>; 4]>,
}

pub fn expected_call<'tcx>(expr: &'tcx Expr<'tcx>) -> Result<CallArgs<'tcx>, Error> {
    match expr.kind {
        ExprKind::Call(fn_item, args) => Ok(CallArgs {
            fn_item: fn_item.hir_id,
            args: args.iter().collect(),
        }),
        ExprKind::MethodCall(_, rec, args, _) => Ok(CallArgs {
            fn_item: expr.hir_id,
            args: iter::once(rec).chain(args).collect(),
        }),
        _ => Err(SpanError::new(SpanErrorKind::ExpectedCall, expr.span).into()),
    }
}

pub fn pat_ident(pat: &Pat<'_>) -> Result<Ident, Error> {
    pat.simple_ident()
        .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedIdentifier, pat.span))
        .map_err(Into::into)
}

pub fn eval_const_val(value: ConstValue) -> Option<u128> {
    match value {
        ConstValue::Scalar(Scalar::Int(scalar)) => eval_scalar_int(scalar),
        _ => None,
    }
}

pub fn eval_val_tree(val_tree: ValTree) -> Option<u128> {
    val_tree.try_to_scalar_int().and_then(eval_scalar_int)
}

pub fn eval_scalar_int(scalar: ScalarInt) -> Option<u128> {
    scalar
        .try_to_u128()
        .ok()
        .or_else(|| scalar.try_to_u64().ok().map(|n| n as u128))
        .or_else(|| scalar.try_to_u32().ok().map(|n| n as u128))
        .or_else(|| scalar.try_to_u16().ok().map(|n| n as u128))
        .or_else(|| scalar.try_to_u8().ok().map(|n| n as u128))
}

macro_rules! args {
    ($e:ident as $( $arg:ident ),+) => {
        let _call_args = utils::expected_call($e)?;
        let [$($arg,)+ ..] = _call_args.args.as_slice() else { panic!("not enough arguments"); };
    };
    ($e:ident as $fn_item:ident with $( $arg:ident ),+) => {
        let _call_args = utils::expected_call($e)?;
        let $fn_item = _call_args.fn_item;
        let [$($arg,)+ ..] = _call_args.args.as_slice() else { panic!("not enough arguments"); };
    };
}

pub(crate) use args;
