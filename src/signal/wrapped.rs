use std::cell::RefCell;

use derive_where::derive_where;
use fhdl_macros::{blackbox, blackbox_ty};

use super::{IntoSignal, Signal, SignalValue};
use crate::{domain::ClockDomain, eval::EvalCtx};

#[derive_where(Debug; T)]
#[blackbox_ty(Wrapped)]
pub struct Wrapped<D: ClockDomain, T: SignalValue>(Signal<D, T>);

impl<D: ClockDomain, T: SignalValue> Clone for Wrapped<D, T> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<D: ClockDomain, T: SignalValue> Wrapped<D, T> {
    pub(super) fn new(signal: Signal<D, T>) -> Self {
        Self(signal)
    }

    #[allow(clippy::should_implement_trait)]
    #[inline]
    #[blackbox(SignalValue)]
    pub fn value(&self) -> T {
        RefCell::borrow(&self.0.next).value()
    }

    pub(crate) fn next(&mut self, ctx: &mut EvalCtx) {
        self.0.next(ctx);
    }
}

impl<D: ClockDomain, T: SignalValue> IntoSignal<D> for Wrapped<D, T> {
    type Value = T;

    #[blackbox(IntoSignal)]
    #[inline]
    fn into_signal(self) -> Signal<D, Self::Value> {
        self.0
    }
}
