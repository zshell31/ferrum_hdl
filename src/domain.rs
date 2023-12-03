use std::marker::PhantomData;

use derive_where::derive_where;
use fhdl_macros::blackbox_ty;

pub const SECOND: usize = 1_000_000_000_000;
pub const MILLISECOND: usize = 1_000_000_000;
pub const MICROSECOND: usize = 1_000_000;
pub const NANOSECOND: usize = 1_000;
pub const PICOSECOND: usize = 1;

pub const fn hz_to_period(freq: usize) -> usize {
    assert!(SECOND >= freq);
    SECOND / freq
}

pub const fn clk_divider<D: ClockDomain>(ps: usize) -> usize {
    assert!(ps >= D::PERIOD);
    ps / D::PERIOD
}

pub trait ClockDomain: 'static {
    /// In hertz
    const FREQ: usize;
    /// In picoseconds
    const PERIOD: usize = hz_to_period(Self::FREQ);
}

pub struct System;

impl ClockDomain for System {
    const FREQ: usize = 100_000_000;
}

pub struct DummySystem;

impl ClockDomain for DummySystem {
    const FREQ: usize = 1;
}

#[derive_where(Debug, Clone, Copy)]
#[blackbox_ty(Clock)]
pub struct Clock<D: ClockDomain> {
    _dom: PhantomData<D>,
}

impl<D: ClockDomain> Default for Clock<D> {
    fn default() -> Self {
        Self { _dom: PhantomData }
    }
}

impl<D: ClockDomain> Clock<D> {
    pub fn new() -> Self {
        Self::default()
    }
}

pub struct TestSystem4;

impl ClockDomain for TestSystem4 {
    const FREQ: usize = 4;
}
