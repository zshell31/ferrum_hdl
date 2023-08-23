use std::{
    fmt::{Debug, Display},
    marker::PhantomData,
    sync::{
        atomic::{AtomicU8, Ordering},
        Arc,
    },
};

use derive_where::derive_where;
use ferrum_macros::blackbox;

use super::domain::ClockDomain;

pub trait SignalValue: Debug + Display + Clone {}

impl SignalValue for bool {}

pub trait Signal<D: ClockDomain>: Sized {
    type Value: SignalValue;

    fn name(&self) -> Option<&'static str> {
        None
    }

    fn next(&mut self) -> Self::Value;

    fn map<O, F>(self, f: F) -> MapSignal<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Value) -> O,
    {
        MapSignal::new(self, f)
    }

    fn clk_cycles(self, clock: Clock<D>) -> impl Iterator<Item = (isize, Self::Value)> {
        SignalClkCycles {
            _dom: PhantomData,
            before: true,
            cycle: -1,
            clock,
            signal: self,
        }
    }
}

pub struct SignalClkCycles<D: ClockDomain, S> {
    _dom: PhantomData<D>,
    before: bool,
    cycle: isize,
    clock: Clock<D>,
    signal: S,
}

impl<D: ClockDomain, S: Signal<D>> Iterator for SignalClkCycles<D, S> {
    type Item = (isize, S::Value);

    fn next(&mut self) -> Option<Self::Item> {
        if self.before {
            self.before = false;
            Some((self.cycle, self.signal.next()))
        } else {
            self.cycle += 1;
            self.clock.next(); // rising
            let _ = self.signal.next();
            self.clock.next(); // falling
            Some((self.cycle, self.signal.next()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct MapSignal<S, F> {
    signal: S,
    f: F,
}

impl<S, F> MapSignal<S, F> {
    fn new(signal: S, f: F) -> Self {
        Self { signal, f }
    }
}

impl<D, S, O, F> Signal<D> for MapSignal<S, F>
where
    D: ClockDomain,
    S: Signal<D>,
    O: SignalValue,
    F: Fn(S::Value) -> O,
{
    type Value = O;

    fn next(&mut self) -> Self::Value {
        let value = self.signal.next();
        (self.f)(value)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum ClockState {
    Init,
    Rising,
    Falling,
}

impl ClockState {
    fn is_rising(&self) -> bool {
        matches!(self, ClockState::Rising)
    }

    fn is_falling(&self) -> bool {
        matches!(self, ClockState::Falling)
    }
}

impl From<u8> for ClockState {
    fn from(value: u8) -> Self {
        match value {
            _ if Self::Init as u8 == value => Self::Init,
            _ if Self::Rising as u8 == value => Self::Rising,
            _ if Self::Falling as u8 == value => Self::Falling,
            _ => unreachable!(),
        }
    }
}

impl From<ClockState> for u8 {
    fn from(value: ClockState) -> Self {
        value as u8
    }
}

#[derive_where(Debug, Clone)]
pub struct Clock<D: ClockDomain> {
    _dom: PhantomData<D>,
    state: Arc<AtomicU8>,
}

impl<D: ClockDomain> Default for Clock<D> {
    fn default() -> Self {
        Self {
            _dom: PhantomData,
            state: Arc::new(AtomicU8::new(ClockState::Init.into())),
        }
    }
}

impl<D: ClockDomain> Clock<D> {
    pub fn new() -> Self {
        Self::default()
    }

    fn state(&self) -> ClockState {
        self.state.load(Ordering::Relaxed).into()
    }

    fn set_state(&self, state: ClockState) {
        self.state.store(state.into(), Ordering::Relaxed)
    }

    pub(crate) fn next(&self) {
        let state = self.state();
        let next_state = match state {
            ClockState::Init => ClockState::Falling,
            ClockState::Rising => ClockState::Falling,
            ClockState::Falling => ClockState::Rising,
        };
        self.set_state(next_state);
    }

    pub fn is_rising(&mut self) -> bool {
        self.state().is_rising()
    }

    pub fn is_falling(&mut self) -> bool {
        self.state().is_falling()
    }
}

#[blackbox(Register, Clone)]
pub struct Register<D: ClockDomain, V: SignalValue> {
    value: V,
    next_value: V,
    clock: Clock<D>,
    comb_fn: Box<dyn Fn(V) -> V>,
}

impl<D: ClockDomain, V: SignalValue> Register<D, V> {
    fn new(clock: Clock<D>, reset_value: V, comb_fn: impl Fn(V) -> V + 'static) -> Self {
        Self {
            value: reset_value.clone(),
            next_value: reset_value,
            clock,
            comb_fn: Box::new(comb_fn),
        }
    }
}

#[blackbox(RegisterFn)]
#[inline(always)]
pub fn reg<D: ClockDomain, V: SignalValue>(
    clock: Clock<D>,
    reset_value: impl Into<V>,
    comb_fn: impl Fn(V) -> V + 'static,
) -> Register<D, V> {
    Register::new(clock, reset_value.into(), comb_fn)
}

impl<D: ClockDomain, V: SignalValue + Display> Signal<D> for Register<D, V> {
    type Value = V;

    fn next(&mut self) -> Self::Value {
        if self.clock.is_rising() {
            self.value = self.next_value.clone();
            self.next_value = (self.comb_fn)(self.value.clone());
        }

        self.value.clone()
    }
}
