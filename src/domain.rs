use std::marker::PhantomData;

use derive_where::derive_where;

pub trait ClockDomain: 'static {
    const PICOSECONDS: usize = 1_000_000_000_000;
    const FREQ: usize;
    const PERIOD: usize = Self::PICOSECONDS / Self::FREQ;
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
