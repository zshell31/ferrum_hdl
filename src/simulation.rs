// use std::collections::HashMap;

// use fnv::FnvBuildHasher;

use std::fmt::Binary;

// type FnvHashMap<K, V> = HashMap<K, V, FnvBuildHasher>;
use crate::signal::SignalValue;

#[derive(Debug, Default)]
pub struct SimCtx {
    cycle_printed: bool,
    cycle: u16,
}

impl SimCtx {
    pub(crate) fn cycle(&self) -> u16 {
        self.cycle
    }

    #[cfg(test)]
    pub(crate) fn next_cycle(&mut self) {
        self.next_cycle_inner()
    }

    fn next_cycle_inner(&mut self) {
        self.cycle_printed = false;
        self.cycle += 1;
    }

    fn print_cycle(&mut self) {
        if !self.cycle_printed {
            println!("\ncycle = {}", self.cycle);
            self.cycle_printed = true;
        }
    }

    pub(crate) fn watch<T: SignalValue>(&mut self, name: &'static str, value: &T) {
        self.print_cycle();
        println!("'{}': {:?}", name, value);
    }

    pub(crate) fn watch_bin<T: SignalValue + Binary>(
        &mut self,
        name: &'static str,
        value: &T,
    ) {
        self.print_cycle();
        println!("'{}': {:b}", name, value);
    }

    pub(crate) fn watcher(&mut self) -> Watcher<'_> {
        Watcher { ctx: self }
    }
}

pub struct Watcher<'a> {
    ctx: &'a mut SimCtx,
}

impl<'a> Watcher<'a> {
    pub fn watch<T: SignalValue>(&mut self, name: &'static str, value: &T) {
        self.ctx.watch(name, value);
    }

    pub fn watch_bin<T: SignalValue + Binary>(&mut self, name: &'static str, value: &T) {
        self.ctx.watch_bin(name, value);
    }
}

pub trait Simulate: Sized {
    type Value;

    fn next(&mut self, ctx: &mut SimCtx) -> Self::Value;

    fn simulate(self) -> Simulation<Self> {
        Simulation {
            ctx: SimCtx::default(),
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
