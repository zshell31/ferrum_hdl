use either::Either;
use ferrum_netlist::{arena::with_arena, sig_ty::SignalTy};
use rustc_const_eval::interpret::{ConstValue, Scalar};
use rustc_middle::ty::{GenericArg, List, ScalarInt, Ty, TyCtxt, UnevaluatedConst};
use rustc_span::Span;
use rustc_type_ir::{
    ConstKind,
    TyKind::{self},
};

use super::Generator;
use crate::{
    blackbox::ItemPath,
    error::{Error, SpanError, SpanErrorKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Generic {
    Ty(SignalTy),
    Const(u128),
}

impl From<SignalTy> for Generic {
    fn from(sig_ty: SignalTy) -> Self {
        Self::Ty(sig_ty)
    }
}

impl From<u128> for Generic {
    fn from(cons: u128) -> Self {
        Self::Const(cons)
    }
}

impl Generic {
    pub fn as_ty(&self) -> Option<&SignalTy> {
        match self {
            Self::Ty(sig_ty) => Some(sig_ty),
            _ => None,
        }
    }

    pub fn as_const<T: TryFrom<u128>>(&self) -> Option<T> {
        match self {
            Self::Const(val) => T::try_from(*val).ok(),
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

    pub fn eval_const_val(value: ConstValue) -> Option<u128> {
        match value {
            ConstValue::Scalar(Scalar::Int(scalar)) => Self::eval_scalar_int(scalar),
            _ => None,
        }
    }

    pub fn resolve_const<'tcx>(
        unevaluated: UnevaluatedConst<'tcx>,
        tcx: TyCtxt<'tcx>,
    ) -> Option<u128> {
        let param_env = tcx.param_env(unevaluated.def);
        let value = tcx
            .const_eval_resolve(param_env, unevaluated.expand(), None)
            .ok()?;

        Self::eval_const_val(value)
    }

    pub fn from_gen_arg<'tcx>(
        arg: &GenericArg<'tcx>,
        generator: &mut Generator<'tcx>,
        span: Span,
    ) -> Result<Self, Error> {
        if let Some(ty) = arg.as_type() {
            let sig_ty = generator.find_sig_ty(ty, None, span)?;
            return Ok(Generic::Ty(sig_ty));
        }

        let cons_val = arg.as_const().and_then(|cons| match cons.kind() {
            ConstKind::Unevaluated(unevaluated) => {
                Self::resolve_const(unevaluated, generator.tcx)
            }
            ConstKind::Value(val_tree) => {
                val_tree.try_to_scalar_int().and_then(Self::eval_scalar_int)
            }
            _ => None,
        });

        cons_val
            .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthGenParam, span).into())
            .map(Generic::Const)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Generics(&'static [Generic]);

impl Generics {
    pub fn get(&self, ind: usize) -> Option<&Generic> {
        self.0.get(ind)
    }

    pub fn as_ty(&self, ind: usize) -> Option<SignalTy> {
        self.get(ind).and_then(|generic| generic.as_ty()).copied()
    }

    pub fn as_const<T: TryFrom<u128>>(&self, ind: usize) -> Option<T> {
        self.get(ind).and_then(|generic| generic.as_const())
    }

    pub fn from_ty<'tcx>(
        ty: &Ty<'tcx>,
        generator: &mut Generator<'tcx>,
        generics: Option<&'tcx List<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<Option<Self>, Error> {
        match ty.kind() {
            TyKind::Array(ty, cons) => {
                let sig_ty: Generic = generator.find_sig_ty(*ty, generics, span)?.into();
                let cons: Generic = cons
                    .try_to_scalar_int()
                    .and_then(Generic::eval_scalar_int)
                    .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthGenParam, span))?
                    .into();

                Ok(Some(Self(unsafe {
                    with_arena().alloc_slice(&[sig_ty, cons])
                })))
            }
            TyKind::Adt(adt, generics) if !generics.is_empty() => {
                // TODO: refactor
                if generator.tcx.def_path(adt.did()) == ItemPath(&["domain", "Clock"]) {
                    return Ok(None);
                }

                let generics = if generator.tcx.def_path(adt.did())
                    == ItemPath(&["signal", "Signal"])
                {
                    Either::Left(generics.iter().skip(1)) // the first generic argument is ClockDomain
                } else {
                    Either::Right(generics.iter())
                };

                Ok(Some(Self(unsafe {
                    with_arena().alloc_from_res_iter(generics.map(|generic| {
                        Generic::from_gen_arg(&generic, generator, span)
                    }))?
                })))
            }
            _ => Ok(None),
        }
    }
}
