use rustc_const_eval::interpret::Scalar;
use rustc_hir::def_id::DefId;
use rustc_middle::{
    mir::{ConstValue, UnevaluatedConst},
    ty::{ParamEnv, ScalarInt, Ty, TyCtxt, TyKind, ValTree},
};

pub fn ty_def_id(ty: Ty<'_>) -> Option<DefId> {
    match ty.kind() {
        TyKind::Adt(adt, _) => Some(adt.did()),
        TyKind::FnDef(did, _) => Some(*did),
        _ => None,
    }
}

pub fn resolve_unevaluated<'tcx>(
    tcx: TyCtxt<'tcx>,
    unevaluated: UnevaluatedConst<'tcx>,
) -> Option<u128> {
    let param_env = ParamEnv::reveal_all();
    let value = tcx.const_eval_resolve(param_env, unevaluated, None);
    let value = value.ok()?;
    eval_const_val(value)
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

pub fn eval_scalar(scalar: Scalar) -> Option<u128> {
    match scalar {
        Scalar::Int(scalar) => eval_scalar_int(scalar),
        _ => None,
    }
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
    ($args:ident as $( $arg:ident ),+) => {
        let [$($arg,)+ ..] = $args else { panic!("not enough arguments"); };
    };
}

pub(crate) use args;
