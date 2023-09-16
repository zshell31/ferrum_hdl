use std::{
    cell::{Cell, RefCell},
    fmt::Debug,
    marker::PhantomData,
    ops::{Add, BitAnd, BitOr, Not, Sub},
    rc::Rc,
};

use derive_where::derive_where;

use crate::{
    bit::{Bit, L},
    domain::{Clock, ClockDomain},
    signal_fn::SignalFn,
    simulation::Simulate,
};

pub trait SignalValue: Debug + Copy + 'static {}

impl SignalValue for bool {}

macro_rules! impl_signal_value_for_tuples {
    (
        $( $t:ident ),+
    ) => {
       impl<$( $t: SignalValue, )+> SignalValue for ($( $t, )+) {}
    };
}

impl_signal_value_for_tuples!(T1);
impl_signal_value_for_tuples!(T1, T2);
impl_signal_value_for_tuples!(T1, T2, T3);
impl_signal_value_for_tuples!(T1, T2, T3, T4);
impl_signal_value_for_tuples!(T1, T2, T3, T4, T5);
impl_signal_value_for_tuples!(T1, T2, T3, T4, T5, T6);
impl_signal_value_for_tuples!(T1, T2, T3, T4, T5, T6, T7);
impl_signal_value_for_tuples!(T1, T2, T3, T4, T5, T6, T7, T8);
impl_signal_value_for_tuples!(T1, T2, T3, T4, T5, T6, T7, T8, T9);
impl_signal_value_for_tuples!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
impl_signal_value_for_tuples!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
impl_signal_value_for_tuples!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);

#[derive_where(Debug, Clone)]
pub struct Signal<D: ClockDomain, T: SignalValue> {
    #[derive_where(skip)]
    _dom: PhantomData<D>,
    next: Rc<RefCell<SignalFn<T>>>,
}

impl<D: ClockDomain, T: SignalValue> Signal<D, T> {
    pub(crate) fn new(f: impl FnMut(u16) -> T + 'static) -> Self {
        Self {
            _dom: PhantomData,
            next: Rc::new(RefCell::new(SignalFn::new(f))),
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub(crate) fn next(&mut self, cycle: u16) -> T {
        self.next.borrow_mut().next_val(cycle)
    }

    pub fn lift(value: T) -> Signal<D, T> {
        Self::new(move |_| value)
    }

    pub fn map<U: SignalValue, F>(self, f: F) -> Signal<D, U>
    where
        F: Fn(T) -> U + Clone + 'static,
    {
        let mut inner = self;
        Signal::new(move |cycle| {
            let val = inner.next(cycle);
            (f)(val)
        })
    }

    pub fn and_then<U: SignalValue, F>(self, f: F) -> Signal<D, U>
    where
        F: Fn(Wrapped<D, T>) -> Signal<D, U> + Clone + 'static,
    {
        let mut wrapped = Wrapped::new(self);
        let mut signal = f(wrapped.clone());
        Signal::new(move |cycle| {
            wrapped.next(cycle);
            signal.next(cycle)
        })
    }

    pub fn source(value: T) -> (Source<T>, Signal<D, T>) {
        let source = Source::new(value);
        let source_clone = source.clone();
        let signal = Signal::new(move |_| source_clone.value());

        (source, signal)
    }
}

impl<D: ClockDomain> Reset<D> {
    pub fn reset() -> (Source<Bit>, Signal<D, Bit>) {
        Self::source(L)
    }

    pub fn click(source: &Source<Bit>, f: impl FnOnce()) {
        source.revert();
        f();
        source.revert();
    }
}

impl<D: ClockDomain, T: SignalValue> Simulate for Signal<D, T> {
    type Value = T;

    fn next(&mut self, cycle: u16) -> Self::Value {
        self.next(cycle)
    }
}

pub struct Wrapped<D: ClockDomain, T: SignalValue>(Signal<D, T>);

impl<D: ClockDomain, T: SignalValue> Clone for Wrapped<D, T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<D: ClockDomain, T: SignalValue> Wrapped<D, T> {
    fn new(signal: Signal<D, T>) -> Self {
        Self(signal)
    }

    #[allow(clippy::should_implement_trait)]
    #[inline(always)]
    pub fn value(&self) -> T {
        self.0.next.borrow().value()
    }

    pub(crate) fn next(&mut self, cycle: u16) {
        self.0.next(cycle);
    }
}

#[derive(Debug, Clone)]
pub struct Source<T: SignalValue>(Rc<Cell<T>>);

impl<T: SignalValue> Source<T> {
    pub(crate) fn new(value: T) -> Self {
        Self(Rc::new(Cell::new(value)))
    }

    pub fn value(&self) -> T {
        self.0.get()
    }

    pub fn set_value(&self, value: T) -> T {
        self.0.replace(value)
    }
}

impl Source<Bit> {
    pub fn revert(&self) {
        self.0.update(|val| !val);
    }
}

pub fn apply2<D: ClockDomain, T: SignalValue, U: SignalValue, V: SignalValue, F>(
    s1: Signal<D, T>,
    s2: Signal<D, U>,
    f: F,
) -> Signal<D, V>
where
    F: Fn(T, U) -> V + Clone + 'static,
{
    let mut s1 = s1;
    let mut s2 = s2;

    Signal::new(move |cycle| {
        let s1 = s1.next(cycle);
        let s2 = s2.next(cycle);
        (f)(s1, s2)
    })
}

impl<D: ClockDomain, T: SignalValue, U: SignalValue> BitAnd<Signal<D, U>> for Signal<D, T>
where
    T: BitAnd<U>,
    <T as BitAnd<U>>::Output: SignalValue,
{
    type Output = Signal<D, <T as BitAnd<U>>::Output>;

    fn bitand(self, rhs: Signal<D, U>) -> Self::Output {
        apply2(self, rhs, |lhs, rhs| lhs.bitand(rhs))
    }
}

impl<D: ClockDomain, T: SignalValue, U: SignalValue> BitOr<Signal<D, U>> for Signal<D, T>
where
    T: BitOr<U>,
    <T as BitOr<U>>::Output: SignalValue,
{
    type Output = Signal<D, <T as BitOr<U>>::Output>;

    fn bitor(self, rhs: Signal<D, U>) -> Self::Output {
        apply2(self, rhs, |lhs, rhs| lhs.bitor(rhs))
    }
}

impl<D: ClockDomain, T: SignalValue> Not for Signal<D, T>
where
    T: Not,
    <T as Not>::Output: SignalValue,
{
    type Output = Signal<D, <T as Not>::Output>;

    fn not(self) -> Self::Output {
        self.map(|val| val.not())
    }
}

impl<D: ClockDomain, T: SignalValue, U: SignalValue> Add<Signal<D, U>> for Signal<D, T>
where
    T: Add<U>,
    <T as Add<U>>::Output: SignalValue,
{
    type Output = Signal<D, <T as Add<U>>::Output>;

    fn add(self, rhs: Signal<D, U>) -> Self::Output {
        apply2(self, rhs, |lhs, rhs| lhs.add(rhs))
    }
}

impl<D: ClockDomain, T: SignalValue, U: SignalValue> Sub<Signal<D, U>> for Signal<D, T>
where
    T: Sub<U>,
    <T as Sub<U>>::Output: SignalValue,
{
    type Output = Signal<D, <T as Sub<U>>::Output>;

    fn sub(self, rhs: Signal<D, U>) -> Self::Output {
        apply2(self, rhs, |lhs, rhs| lhs.sub(rhs))
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

#[inline(always)]
pub fn reg<D: ClockDomain, T: SignalValue>(
    _clock: Clock<D>,
    rst_val: T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let mut next_val = rst_val;
    Signal::new(move |_| {
        let value = next_val;
        next_val = (comb_fn)(value);

        value
    })
}

#[allow(type_alias_bounds)]
pub type Reset<D: ClockDomain> = Signal<D, Bit>;

#[inline(always)]
pub fn reg_rst<D: ClockDomain, T: SignalValue>(
    _clock: Clock<D>,
    mut rst: Reset<D>,
    rst_val: T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let mut next_val = rst_val;
    Signal::new(move |cycle| {
        let value = if rst.next(cycle).into() {
            rst_val
        } else {
            next_val
        };
        next_val = (comb_fn)(value);

        value
    })
}

pub trait Bundle<D: ClockDomain, T: SignalValue>
where
    Self: SignalValue,
{
    type Unbundled;

    fn bundle(signals: Self::Unbundled) -> Signal<D, Self>;

    fn unbundle(signal: Signal<D, Self>) -> Self::Unbundled;
}

#[cfg(test)]
mod tests {
    use super::{SignalIterExt, *};
    use crate::unsigned::Unsigned;

    struct TestSystem;

    impl ClockDomain for TestSystem {
        const FREQ: usize = 4;
    }

    #[test]
    fn test_iter() {
        let s = [0, 4, 3, 1, 2]
            .into_iter()
            .map(Unsigned::<8>::from)
            .into_signal::<TestSystem>();

        assert_eq!(s.values().take(5).collect::<Vec<_>>(), [0, 4, 3, 1, 2]);
    }

    #[test]
    fn test_reg() {
        let clk = Clock::<TestSystem>::default();
        let r = reg::<TestSystem, Unsigned<3>>(clk, 0.into(), |val| val + 1);

        assert_eq!(r.values().take(16).collect::<Vec<_>>(), [
            0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7
        ]);
    }
}
