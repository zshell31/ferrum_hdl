use either::Either;
use fhdl_blackbox::BlackboxTy;
use fhdl_netlist::{
    arena::with_arena,
    sig_ty::{ConstParam, SignalTy},
};
use rustc_middle::ty::{
    ConstKind, GenericArg, GenericArgKind, List, ParamEnv, Ty, TyCtxt, TyKind,
    UnevaluatedConst,
};
use rustc_span::Span;

use super::{GenMode, Generator};
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    utils,
};

trait GenericArgExt {
    fn is_param(&self) -> bool;
}

impl<'tcx> GenericArgExt for GenericArg<'tcx> {
    fn is_param(&self) -> bool {
        match self.unpack() {
            GenericArgKind::Const(cons) => {
                if let ConstKind::Param(_) = cons.kind() {
                    return true;
                }
            }
            GenericArgKind::Type(ty) => {
                if let TyKind::Param(_) = ty.kind() {
                    return true;
                }
            }
            _ => {}
        }

        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Generic {
    Ty(SignalTy),
    Const(ConstParam),
    Ignored,
}

impl From<SignalTy> for Generic {
    fn from(sig_ty: SignalTy) -> Self {
        Self::Ty(sig_ty)
    }
}

impl From<ConstParam> for Generic {
    fn from(cons: ConstParam) -> Self {
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

    pub fn as_const(&self) -> Option<ConstParam> {
        match self {
            Self::Const(val) => Some(*val),
            _ => None,
        }
    }

    pub fn resolve_const<'tcx>(
        unevaluated: UnevaluatedConst<'tcx>,
        tcx: TyCtxt<'tcx>,
    ) -> Option<ConstParam> {
        use rustc_middle::mir::UnevaluatedConst;

        if unevaluated.args.iter().any(|arg| arg.is_param()) {
            println!("{:?}", unevaluated);
            return Some(ConstParam::EvalGen);
        }

        let param_env = ParamEnv::reveal_all();
        let value = tcx.const_eval_resolve(
            param_env.without_caller_bounds(),
            UnevaluatedConst::new(unevaluated.def, unevaluated.args),
            None,
        );

        let value = value.ok()?;
        utils::eval_const_val(value).map(Into::into)
    }

    pub fn from_gen_arg<'tcx>(
        generator: &mut Generator<'tcx>,
        arg: &GenericArg<'tcx>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<Self, Error> {
        if let Some(ty) = arg.as_type() {
            let sig_ty =
                generator.find_sig_ty(ty, &ctx.with_generic_args(List::empty()), span)?;
            return Ok(Generic::Ty(sig_ty));
        }

        let cons_val = arg.as_const().and_then(|cons| {
            println!("const_val: {:?}", cons.kind());
            match cons.kind() {
                ConstKind::Unevaluated(unevaluated) => {
                    Self::resolve_const(unevaluated, generator.tcx).map(Into::into)
                }
                ConstKind::Value(val_tree) => {
                    utils::eval_val_tree(val_tree).map(Into::into)
                }
                ConstKind::Param(param) => Some(ConstParam::Generic(param.index)),
                _ => None,
            }
        });

        cons_val
            .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthGenParam, span).into())
            .map(Generic::Const)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Generics {
    mode: GenMode,
    generics: &'static [Generic],
}

impl Generics {
    pub fn get(&self, ind: usize) -> Option<&Generic> {
        self.generics.get(ind)
    }

    pub fn as_ty(&self, ind: usize) -> Option<SignalTy> {
        self.get(ind).and_then(|generic| generic.as_ty()).copied()
    }

    pub fn as_const(&self, ind: usize) -> Option<ConstParam> {
        let param = self.get(ind).and_then(|generic| generic.as_const());
        match self.mode {
            GenMode::Crate(_) => param.filter(|param| param.is_value()),
            GenMode::Fhdl => param,
        }
    }

    pub fn from_ty<'tcx>(
        ty: &Ty<'tcx>,
        generator: &mut Generator<'tcx>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<Option<Self>, Error> {
        println!("ty {:?} def_id {:?}", ty.kind(), utils::ty_def_id(*ty));
        match ty.kind() {
            TyKind::Array(ty, cons) => {
                let sig_ty: Generic = generator.find_sig_ty(*ty, ctx, span)?.into();
                let cons: Generic = ConstParam::from(
                    cons.try_eval_scalar_int(generator.tcx, ParamEnv::reveal_all())
                        .and_then(utils::eval_scalar_int)
                        .ok_or_else(|| {
                            SpanError::new(SpanErrorKind::NotSynthGenParam, span)
                        })?,
                )
                .into();

                Ok(Some(Self {
                    mode: generator.mode,
                    generics: unsafe { with_arena().alloc_slice(&[sig_ty, cons]) },
                }))
            }
            TyKind::Adt(adt, generics) if !generics.is_empty() => {
                let blackbox_ty = generator.find_blackbox_ty(adt.did());
                if let Some(BlackboxTy::Clock) = blackbox_ty {
                    return Ok(None);
                }

                let generics = match blackbox_ty {
                    Some(BlackboxTy::Signal | BlackboxTy::Wrapped) => {
                        // the first generic argument is ClockDomain,
                        Either::Left([None, Some(generics[1])].into_iter())
                    }
                    _ => Either::Right(generics.iter().map(Some)),
                };

                Self::from_args(generator, generics, ctx, span).map(Some)
            }
            _ => Ok(None),
        }
    }

    pub fn from_args<'tcx>(
        generator: &mut Generator<'tcx>,
        generic_args: impl IntoIterator<Item = Option<GenericArg<'tcx>>>,
        ctx: &EvalContext<'tcx>,
        span: Span,
    ) -> Result<Self, Error> {
        Ok(Self {
            mode: generator.mode,
            generics: unsafe {
                with_arena().alloc_from_res_iter(generic_args.into_iter().map(
                    |generic| match generic {
                        Some(generic) => {
                            Generic::from_gen_arg(generator, &generic, ctx, span)
                        }
                        None => Ok(Generic::Ignored),
                    },
                ))?
            },
        })
    }
}
