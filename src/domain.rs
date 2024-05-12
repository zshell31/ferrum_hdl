use std::{
    cell::Cell,
    fmt::{self, Display},
    io,
    marker::{ConstParamTy, PhantomData},
    rc::Rc,
};

use derive_where::derive_where;
use fhdl_macros::blackbox_ty;
use vcd::IdCode;

use crate::trace::{TraceTy, TraceVars, Traceable, Tracer};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, ConstParamTy)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, ConstParamTy)]
pub enum Polarity {
    ActiveHigh = 1,
    ActiveLow = 2,
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

#[derive(Debug, Clone, Copy)]
enum ClockState {
    Rising,
    Falling,
}

impl Display for ClockState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Rising => "rising",
            Self::Falling => "falling",
        })
    }
}

#[derive_where(Debug, Clone)]
#[blackbox_ty(Clock)]
pub struct Clock<D: ClockDomain> {
    state: Rc<Cell<ClockState>>,
    _dom: PhantomData<D>,
}

impl<D: ClockDomain> Display for Clock<D> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.state.get().fmt(f)
    }
}

impl<D: ClockDomain> Default for Clock<D> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<D: ClockDomain> Clock<D> {
    pub fn new() -> Self {
        Self {
            state: Rc::new(Cell::new(ClockState::Falling)),
            _dom: PhantomData,
        }
    }

    pub fn is_rising(&self) -> bool {
        matches!(self.state.get(), ClockState::Rising)
    }

    pub fn is_falling(&self) -> bool {
        matches!(self.state.get(), ClockState::Falling)
    }

    pub fn invert(&self) {
        self.state.update(|state| match state {
            ClockState::Rising => ClockState::Falling,
            ClockState::Falling => ClockState::Rising,
        });
    }
}

impl<D: ClockDomain> Traceable for Clock<D> {
    fn add_vars(vars: &mut TraceVars) {
        vars.add_ty(TraceTy::Wire);
    }

    fn trace(&self, id: &mut IdCode, tracer: &mut Tracer) -> io::Result<()> {
        match self.state.get() {
            ClockState::Rising => tracer.change_wire(id, vcd::Value::V1),
            ClockState::Falling => tracer.change_wire(id, vcd::Value::V0),
        }
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
