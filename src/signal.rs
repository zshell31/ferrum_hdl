// use crate::traits::Synthesizable;

// use rustc_hir::Lit;
use std::{fmt::Display, marker::PhantomData};

use ferrum_macros::blackbox;

use super::domain::ClockDomain;

pub trait SignalValue: Clone {}

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

    fn iter(self) -> SignalIter<D, Self> {
        SignalIter {
            _dom: PhantomData,
            signal: self,
        }
    }
}

pub struct SignalIter<D, S> {
    _dom: PhantomData<D>,
    signal: S,
}

impl<D: ClockDomain, S: Signal<D>> Iterator for SignalIter<D, S> {
    type Item = S::Value;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.signal.next())
    }
}

#[derive(Debug)]
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

impl<D: ClockDomain, T: SignalValue> Signal<D> for T {
    type Value = Self;

    fn next(&mut self) -> Self::Value {
        self.clone()
    }
}

#[blackbox(Clock)]
#[derive(Debug)]
pub struct Clock<D: ClockDomain> {
    _dom: PhantomData<D>,
    value: Option<bool>,
}

// impl<D: ClockDomain> Synthesizable for Clock<D> {
//     fn width() -> u128 {
//         1
//     }

//     fn synthesize_lit(_: &Lit) -> Option<Expression> {
//         None
//     }
// }

impl<D: ClockDomain> Default for Clock<D> {
    fn default() -> Self {
        Self {
            _dom: PhantomData,
            value: None,
        }
    }
}

impl<D: ClockDomain> Clock<D> {
    pub fn value(&self) -> bool {
        self.value.unwrap_or_default()
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_rising(&mut self) -> bool {
        let old = self.value();
        let new = self.next();
        !old && new
    }

    pub fn is_falling(&mut self) -> bool {
        let old = self.value();
        let new = self.next();
        old && !new
    }
}

impl<D: ClockDomain> Signal<D> for Clock<D> {
    type Value = bool;

    fn next(&mut self) -> Self::Value {
        match self.value.as_mut() {
            Some(value) => {
                *value = !(*value);
            }
            None => {
                self.value = Some(false);
            }
        }

        self.value.unwrap()
    }
}

#[blackbox(Register)]
pub struct Register<D: ClockDomain, V: SignalValue> {
    reset_value: V,
    prev_value: V,
    next_value: Option<V>,
    clock: Clock<D>,
    signal_fn: Box<dyn Fn(V) -> V>,
}

impl<D: ClockDomain, V: SignalValue> Register<D, V> {
    fn new(
        clock: Clock<D>,
        reset_value: V,
        signal_fn: impl Fn(V) -> V + 'static,
    ) -> Self {
        let prev_value = reset_value.clone();

        Self {
            reset_value,
            prev_value,
            next_value: None,
            clock,
            signal_fn: Box::new(signal_fn),
        }
    }
}

// impl<V: SignalValue + Synthesizable> Synthesizable for Register<DummySystem, V> {
//     fn width() -> u128 {
//         V::width()
//     }

//     fn synthesize_lit(lit: &Lit) -> Option<Expression> {
//         V::synthesize_lit(lit)
//     }
// }

#[blackbox(RegisterFn)]
#[inline(always)]
pub fn register<D: ClockDomain, V: SignalValue>(
    clock: Clock<D>,
    reset_value: impl Into<V>,
    signal_fn: impl Fn(V) -> V + 'static,
) -> Register<D, V> {
    Register::new(clock, reset_value.into(), signal_fn)
}

#[derive(Debug, Clone, Copy)]
pub struct RegisterFn<D: ClockDomain, V: SignalValue>((PhantomData<D>, PhantomData<V>));

// impl<V: SignalValue + Synthesizable> Synthesizable for RegisterFn<DummySystem, V> {
//     fn width() -> u128 {
//         V::width()
//     }

//     fn synthesize_lit(lit: &Lit) -> Option<Expression> {
//         V::synthesize_lit(lit)
//     }
// }

impl<D: ClockDomain, V: SignalValue + Display> Signal<D> for Register<D, V> {
    type Value = V;

    fn next(&mut self) -> Self::Value {
        if self.clock.is_rising() {
            self.prev_value = self
                .next_value
                .clone()
                .unwrap_or_else(|| self.reset_value.clone());
            self.next_value
                .replace((self.signal_fn)(self.prev_value.clone()));
        }
        self.prev_value.clone()
    }
}
