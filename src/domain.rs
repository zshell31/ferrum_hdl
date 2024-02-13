use std::marker::PhantomData;

use derive_where::derive_where;
use fhdl_macros::blackbox_ty;

use crate::bit::{self, Bit};

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

// Clock is made as the non zero sized type to generate the netlist correctly
#[derive_where(Debug, Clone, Copy)]
#[blackbox_ty(Clock)]
pub struct Clock<D: ClockDomain> {
    _bit: Bit,
    _dom: PhantomData<D>,
}

impl<D: ClockDomain> Default for Clock<D> {
    fn default() -> Self {
        Self {
            _bit: bit::L,
            _dom: PhantomData,
        }
    }
}

impl<D: ClockDomain> Clock<D> {
    pub fn new() -> Self {
        Self::default()
    }
}

pub struct TestDomain<const N: usize>;

impl<const N: usize> ClockDomain for TestDomain<N> {
    const FREQ: usize = N;
}

pub type TD4 = TestDomain<4>;
pub type TD8 = TestDomain<8>;
pub type TD16 = TestDomain<16>;
