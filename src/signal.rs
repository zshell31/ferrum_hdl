use std::{fmt::Debug, marker::PhantomData, rc::Rc};

use derive_where::derive_where;
use ferrum_netlist::sig_ty::IsPrimTy;

use super::domain::ClockDomain;

pub trait SignalValue: Debug + Clone {}

impl SignalValue for bool {}

impl<T1: SignalValue, T2: SignalValue> SignalValue for (T1, T2) {}
impl<T1: SignalValue, T2: SignalValue, T3: SignalValue> SignalValue for (T1, T2, T3) {}

pub trait Signal<D: ClockDomain>: Sized {
    type Value: SignalValue;

    fn name(&self) -> Option<&'static str> {
        None
    }

    fn next(&mut self) -> Self::Value;

    fn smap<O, F>(self, f: F) -> MapSignal<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Value) -> O,
    {
        MapSignal::new(self, f)
    }

    fn iter(self) -> impl Iterator<Item = Self::Value> {
        SignalIter {
            _dom: PhantomData,
            signal: self,
        }
    }
}

impl<D: ClockDomain, S1: Signal<D>, S2: Signal<D>> Signal<D> for (S1, S2) {
    type Value = (S1::Value, S2::Value);

    fn next(&mut self) -> Self::Value {
        (self.0.next(), self.1.next())
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

#[derive(Clone)]
pub struct Apply2<S1, S2, F> {
    s1: S1,
    s2: S2,
    f: F,
}

pub fn apply2<D, S1, S2, O, F>(s1: S1, s2: S2, f: F) -> Apply2<S1, S2, F>
where
    D: ClockDomain,
    S1: Signal<D>,
    S2: Signal<D>,
    O: SignalValue,
    F: Fn(S1::Value, S2::Value) -> O,
{
    Apply2 { s1, s2, f }
}

impl<D, S1, S2, O, F> Signal<D> for Apply2<S1, S2, F>
where
    D: ClockDomain,
    S1: Signal<D>,
    S2: Signal<D>,
    O: SignalValue,
    F: Fn(S1::Value, S2::Value) -> O,
{
    type Value = O;

    fn next(&mut self) -> Self::Value {
        let s1 = self.s1.next();
        let s2 = self.s2.next();
        (self.f)(s1, s2)
    }
}

pub trait SignalIterExt: IntoIterator + Sized {
    fn into_signal(self) -> IterSignal<Self::IntoIter> {
        IterSignal(self.into_iter())
    }
}

impl<I> SignalIterExt for I
where
    I: IntoIterator,
    I::Item: SignalValue,
{
}

pub struct IterSignal<I>(I);

impl<D: ClockDomain, I: Iterator> Signal<D> for IterSignal<I>
where
    I::Item: SignalValue,
{
    type Value = I::Item;

    fn next(&mut self) -> Self::Value {
        Iterator::next(&mut self.0).expect("No values")
    }
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

#[derive_where(Clone)]
pub struct Register<D: ClockDomain, V: SignalValue> {
    value: V,
    next_value: V,
    _clock: Clock<D>,
    comb_fn: Rc<dyn Fn(V) -> V>,
}

impl<D: ClockDomain, V: SignalValue> Register<D, V> {
    fn new(clock: Clock<D>, reset_value: V, comb_fn: impl Fn(V) -> V + 'static) -> Self {
        Self {
            value: reset_value.clone(),
            next_value: reset_value,
            _clock: clock,
            comb_fn: Rc::new(comb_fn),
        }
    }
}

#[inline(always)]
pub fn reg<D: ClockDomain, V: SignalValue + IsPrimTy>(
    clock: Clock<D>,
    reset_value: impl Into<V>,
    comb_fn: impl Fn(V) -> V + 'static,
) -> Register<D, V> {
    Register::new(clock, reset_value.into(), comb_fn)
}

impl<D: ClockDomain, V: SignalValue> Signal<D> for Register<D, V> {
    type Value = V;

    fn next(&mut self) -> Self::Value {
        self.value = self.next_value.clone();
        self.next_value = (self.comb_fn)(self.value.clone());

        self.value.clone()
    }
}
