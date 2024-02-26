use std::borrow::Borrow;

use fhdl_macros::{blackbox, synth};

use super::{Signal, SignalValue, Source};
use crate::domain::{Clock, ClockDomain};

#[allow(type_alias_bounds)]
pub type Reset<D: ClockDomain> = Signal<D, bool>;

impl<D: ClockDomain> Reset<D> {
    #[synth(inline)]
    pub fn reset() -> Self {
        Self::lift(false)
    }

    pub fn reset_src() -> (Source<bool>, Self) {
        Self::source(false)
    }
}

#[allow(type_alias_bounds)]
pub type Enable<D: ClockDomain> = Signal<D, bool>;

impl<D: ClockDomain> Enable<D> {
    #[synth(inline)]
    pub fn enable() -> Self {
        Self::lift(true)
    }

    pub fn enable_src() -> (Source<bool>, Self) {
        Self::source(true)
    }
}

#[blackbox(SignalReg)]
pub fn reg<D: ClockDomain, T: SignalValue>(
    _clock: Clock<D>,
    rst: &Reset<D>,
    rst_val: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let mut rst = rst.clone();
    let rst_val = rst_val.borrow().clone();

    let mut next_val = rst_val.clone();
    Signal::new(move |ctx| {
        if rst.next(ctx) {
            // Asynchronous reset
            next_val = rst_val.clone();
            next_val.clone()
        } else {
            let val = next_val.clone();
            next_val = (comb_fn)(val.clone());
            val
        }
    })
}

#[synth(inline)]
#[inline]
pub fn reg0<D: ClockDomain, T: SignalValue + Default>(
    clk: Clock<D>,
    rst: &Reset<D>,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let reg = reg(clk, rst, &T::default(), comb_fn);
    reg
}

#[blackbox(SignalRegEn)]
pub fn reg_en<D: ClockDomain, T: SignalValue>(
    _clock: Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    rst_val: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let mut rst = rst.clone();
    let mut en = en.clone();
    let rst_val = rst_val.borrow().clone();

    let mut next_val = rst_val.clone();
    Signal::new(move |ctx| {
        if rst.next(ctx) {
            // Asynchronous reset
            next_val = rst_val.clone();
            next_val.clone()
        } else if en.next(ctx) {
            let val = next_val.clone();
            next_val = (comb_fn)(val.clone());
            val
        } else {
            let val = next_val.clone();
            (comb_fn)(val.clone());
            val
        }
    })
}

#[synth(inline)]
#[inline]
pub fn reg_en0<D: ClockDomain, T: SignalValue + Default>(
    clk: Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let reg = reg_en(clk, rst, en, &T::default(), comb_fn);
    reg
}
