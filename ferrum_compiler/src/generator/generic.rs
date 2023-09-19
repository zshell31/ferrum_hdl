use either::Either;
use ferrum_netlist::{arena::with_arena, sig_ty::SignalTy};
use rustc_middle::ty::{
    GenericArg, GenericArgsRef, List, ParamEnv, Ty, TyCtxt, UnevaluatedConst,
};
use rustc_span::Span;
use rustc_type_ir::{
    ConstKind,
    TyKind::{self},
};

use super::Generator;
use crate::{
    blackbox::ItemPath,
    error::{Error, SpanError, SpanErrorKind},
    utils,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Generic {
    Ty(SignalTy),
    Const(u128),
    Ignored,
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

    pub fn resolve_const<'tcx>(
        unevaluated: UnevaluatedConst<'tcx>,
        tcx: TyCtxt<'tcx>,
    ) -> Option<u128> {
        let param_env = ParamEnv::reveal_all();
        let value = tcx.const_eval_resolve(
            param_env.without_caller_bounds(),
            unevaluated.expand(),
            None,
        );

        let value = value.ok()?;
        utils::eval_const_val(value)
    }

    pub fn from_gen_arg<'tcx>(
        arg: &GenericArg<'tcx>,
        generator: &mut Generator<'tcx>,
        span: Span,
    ) -> Result<Self, Error> {
        if let Some(ty) = arg.as_type() {
            let sig_ty = generator.find_sig_ty(ty, List::empty(), span)?;
            return Ok(Generic::Ty(sig_ty));
        }

        let cons_val = arg.as_const().and_then(|cons| match cons.kind() {
            ConstKind::Unevaluated(unevaluated) => {
                Self::resolve_const(unevaluated, generator.tcx)
            }
            ConstKind::Value(val_tree) => utils::eval_val_tree(val_tree),
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
        generics: GenericArgsRef<'tcx>,
        span: Span,
    ) -> Result<Option<Self>, Error> {
        match ty.kind() {
            TyKind::Array(ty, cons) => {
                let sig_ty: Generic = generator.find_sig_ty(*ty, generics, span)?.into();
                let cons: Generic = cons
                    .try_to_scalar_int()
                    .and_then(utils::eval_scalar_int)
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

                let def_path = generator.tcx.def_path(adt.did());
                let generics = if def_path == ItemPath(&["signal", "Signal"])
                    || def_path == ItemPath(&["signal", "Wrapped"])
                {
                    Either::Left([None, Some(generics[1])].into_iter()) // the first generic argument is ClockDomain
                } else {
                    Either::Right(generics.iter().map(Some))
                };

                Self::from_args(generator, generics, span).map(Some)
            }
            _ => Ok(None),
        }
    }

    pub fn from_args<'tcx>(
        generator: &mut Generator<'tcx>,
        generic_args: impl IntoIterator<Item = Option<GenericArg<'tcx>>>,
        span: Span,
    ) -> Result<Self, Error> {
        Ok(Self(unsafe {
            with_arena().alloc_from_res_iter(generic_args.into_iter().map(|generic| {
                match generic {
                    Some(generic) => Generic::from_gen_arg(&generic, generator, span),
                    None => Ok(Generic::Ignored),
                }
            }))?
        }))
    }
}
