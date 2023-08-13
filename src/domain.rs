pub trait ClockDomain {
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
