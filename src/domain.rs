use std::{
    fmt::{self, Display},
    marker::PhantomData,
};

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

#[derive(Debug, Clone, Copy)]
pub enum SyncKind {
    Sync,
    Async,
}

impl SyncKind {
    pub fn from_val(val: u128) -> Option<Self> {
        match val {
            val if val == SyncKind::Sync as u128 => Some(SyncKind::Sync),
            val if val == SyncKind::Async as u128 => Some(SyncKind::Async),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Polarity {
    ActiveHigh = 1,
    ActiveLow = 20,
}

impl Display for Polarity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::ActiveHigh => "posedge",
            Self::ActiveLow => "negedge",
        })
    }
}

impl Polarity {
    pub fn from_val(val: u128) -> Option<Self> {
        match val {
            val if val == Polarity::ActiveHigh as u128 => Some(Polarity::ActiveHigh),
            val if val == Polarity::ActiveLow as u128 => Some(Polarity::ActiveLow),
            _ => None,
        }
    }

    pub fn bool(&self, b: bool) -> bool {
        match self {
            Self::ActiveHigh => b,
            Self::ActiveLow => !b,
        }
    }
}

pub trait ClockDomain: 'static {
    /// In hertz
    const FREQ: usize;
    /// In picoseconds
    const PERIOD: usize = hz_to_period(Self::FREQ);
    /// Async/Sync reset
    const RESET_KIND: SyncKind;
    /// Reset Polarity
    const RESET_POLARITY: Polarity;
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
    const RESET_KIND: SyncKind = SyncKind::Sync;
    const RESET_POLARITY: Polarity = Polarity::ActiveHigh;
}

pub type TD4 = TestDomain<4>;
pub type TD8 = TestDomain<8>;
pub type TD16 = TestDomain<16>;
