use std::{cell::RefCell, rc::Rc};

use super::{Signal, SignalValue};
use crate::{
    bit::Bit,
    domain::ClockDomain,
    eval::{Eval, EvalCtx},
};

impl<D: ClockDomain, T: SignalValue> Eval<D> for Signal<D, T> {
    type Value = T;

    fn next(&mut self, ctx: &mut EvalCtx) -> Self::Value {
        self.next(ctx)
    }
}

impl<D: ClockDomain, T: SignalValue> Signal<D, T> {
    pub fn source(value: T) -> (Source<T>, Signal<D, T>) {
        let source = Source::new(value);
        let source_clone = source.clone();
        let signal = Signal::new(move |_| source_clone.value());

        (source, signal)
    }
}

#[derive(Debug, Clone)]
pub struct Source<T: SignalValue>(Rc<RefCell<T>>);

impl<T: SignalValue> Source<T> {
    pub(crate) fn new(value: T) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }

    pub fn value(&self) -> T {
        RefCell::borrow(&self.0).clone()
    }

    pub fn set_value(&self, value: T) -> T {
        self.0.replace(value)
    }

    pub fn with(&self, f: impl FnOnce(T) -> T) {
        let value = self.value();
        self.set_value(f(value));
    }
}

impl Source<Bit> {
    pub fn invert(&self) {
        self.0.replace_with(|val| !(*val));
    }
}

pub trait SignalIterExt: IntoIterator + Sized
where
    Self::Item: SignalValue,
{
    fn into_signal<D: ClockDomain>(self) -> Signal<D, Self::Item>;
}

impl<I> SignalIterExt for I
where
    I: IntoIterator + Sized,
    I::IntoIter: 'static,
    I::Item: SignalValue,
{
    fn into_signal<D: ClockDomain>(self) -> Signal<D, Self::Item> {
        let mut iter = self.into_iter();
        Signal::new(move |_| iter.next().expect("No values"))
    }
}
