use std::collections::HashMap;

use fnv::FnvBuildHasher;

type FnvHashMap<K, V> = HashMap<K, V, FnvBuildHasher>;

use crate::signal::SignalValue;

#[derive(Debug, Default)]
pub struct SimCtx {
    cycle_printed: bool,
    cycle: u16,
    names: FnvHashMap<&'static str, usize>,
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

    pub(crate) fn watch<T: SignalValue>(
        &mut self,
        signal_id: usize,
        name: &'static str,
        value: &T,
    ) {
        if !self.cycle_printed {
            println!("\ncycle = {}", self.cycle);
            self.cycle_printed = true;
        }
        match self.names.get(&name) {
            Some(id) => {
                if *id != signal_id {
                    panic!("Already registered signal with name '{}'", name);
                }
            }
            None => {
                self.names.insert(name, signal_id);
            }
        };

        println!("['{}': {:?}]", name, value);
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
