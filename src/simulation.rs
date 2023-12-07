use crate::{signal::SignalValue, watchable::Formatter};

#[derive(Debug)]
pub struct SimCtx {
    cycle_printed: bool,
    cycle: usize,
}

impl SimCtx {
    pub(crate) fn new() -> Self {
        Self {
            cycle_printed: false,
            cycle: usize::MAX,
        }
    }
    pub(crate) fn cycle(&self) -> usize {
        self.cycle
    }

    #[cfg(test)]
    pub(crate) fn next_cycle(&mut self) {
        self.next_cycle_inner()
    }

    fn next_cycle_inner(&mut self) {
        self.cycle_printed = false;
        self.cycle = self.cycle.wrapping_add(1);
    }

    fn print_cycle(&mut self) {
        if !self.cycle_printed {
            println!("\ncycle = {}", self.cycle);
            self.cycle_printed = true;
        }
    }

    pub(crate) fn watch<T: SignalValue>(&mut self, value: &T, fmt: &Formatter<T>) {
        self.print_cycle();
        fmt.output(value);
    }
}

pub trait Simulate: Sized {
    type Value;

    fn next(&mut self, ctx: &mut SimCtx) -> Self::Value;

    fn simulate(self) -> Simulation<Self> {
        Simulation {
            ctx: SimCtx::new(),
            source: self,
        }
    }
}

#[derive(Debug)]
pub struct Simulation<S> {
    ctx: SimCtx,
    source: S,
}

impl<S: Simulate> Simulation<S> {
    pub fn next_cycle(&mut self) -> S::Value {
        self.next().unwrap()
    }
}

impl<S: Simulate> Iterator for Simulation<S> {
    type Item = S::Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.ctx.next_cycle_inner();
        Some(self.source.next(&mut self.ctx))
    }
}
