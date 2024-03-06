use std::borrow::Borrow;

use fhdl_macros::{blackbox, synth};

use super::{Signal, SignalValue, Source};
use crate::domain::{Clock, ClockDomain, Polarity, SyncKind};

#[allow(type_alias_bounds)]
pub type Reset<D: ClockDomain> = Signal<D, bool>;

impl<D: ClockDomain> Reset<D> {
    #[synth(inline)]
    pub fn reset() -> Self {
        let rst = Self::lift(false);
        rst
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
        let en = Self::lift(true);
        en
    }

    pub fn enable_src() -> (Source<bool>, Self) {
        Self::source(true)
    }
}

#[synth(inline)]
#[inline]
pub fn reg<D: ClockDomain, T: SignalValue>(
    clk: Clock<D>,
    rst: &Reset<D>,
    init: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let en = Enable::enable();
    let reg = reg_en(clk, rst, &en, init, comb_fn);
    reg
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

#[synth(inline)]
#[inline]
pub fn reg_en<D: ClockDomain, T: SignalValue>(
    clk: Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    init: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let reg = dff::<D, T>(
        clk,
        rst,
        en,
        init,
        comb_fn,
        D::RESET_KIND,
        D::RESET_POLARITY,
    );
    reg
}

#[blackbox(SignalReg)]
pub fn dff<D: ClockDomain, T: SignalValue>(
    _clock: Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    init: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
    _rst_kind: SyncKind,
    _rst_polarity: Polarity,
) -> Signal<D, T> {
    let mut rst = rst.clone();
    let mut en = en.clone();
    let init = init.borrow().clone();

    let mut next_val = init.clone();
    Signal::new(move |ctx| {
        if rst.next(ctx) {
            // Asynchronous reset
            next_val = init.clone();
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
