use std::{
    fmt::Debug,
    marker::PhantomData,
    ops::{Add, BitAnd, BitOr, Not, Sub},
};

use dyn_clone::{clone_trait_object, DynClone};
use ferrum_netlist::sig_ty::IsPrimTy;

use crate::domain::{Clock, ClockDomain};

pub trait SignalValue: Debug + Clone + 'static {}

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

pub(crate) trait SignalFn<T: SignalValue>:
    DynClone + FnMut() -> T + 'static
{
}

impl<T: SignalValue, F> SignalFn<T> for F where F: FnMut() -> T + Clone + 'static {}

clone_trait_object!(<T> SignalFn<T> where T: SignalValue);

pub struct Signal<D: ClockDomain, T: SignalValue> {
    _dom: PhantomData<D>,
    next: Box<dyn SignalFn<T>>,
}

impl<D: ClockDomain, T: SignalValue> Clone for Signal<D, T> {
    fn clone(&self) -> Self {
        Self {
            _dom: PhantomData,
            next: self.next.clone(),
        }
    }
}

impl<D: ClockDomain, T: SignalValue> Signal<D, T> {
    pub(crate) fn new(f: impl SignalFn<T>) -> Self {
        Self {
            _dom: PhantomData,
            next: Box::new(f),
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> T {
        (self.next)()
    }

    pub fn map<U: SignalValue, F>(self, f: F) -> Signal<D, U>
    where
        F: Fn(T) -> U + Clone + 'static,
    {
        let mut next = self.next;
        Signal::new(move || {
            let val = (next)();
            (f)(val)
        })
    }

    pub fn iter(self) -> impl Iterator<Item = T> {
        SignalIter(self)
    }
}

pub struct SignalIter<S>(S);

impl<D: ClockDomain, T: SignalValue> Iterator for SignalIter<Signal<D, T>> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.0.next())
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
    let mut s1 = s1.next;
    let mut s2 = s2.next;

    Signal::new(move || {
        let s1 = (s1)();
        let s2 = (s2)();
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
    I::IntoIter: Clone + 'static,
    I::Item: SignalValue,
{
    fn into_signal<D: ClockDomain>(self) -> Signal<D, Self::Item> {
        let mut iter = self.into_iter();
        Signal::new(move || iter.next().expect("No values"))
    }
}

#[inline(always)]
pub fn reg<D: ClockDomain, T: SignalValue + IsPrimTy>(
    _clock: Clock<D>,
    reset_val: T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let mut next_value = reset_val.clone();
    Signal::new(move || {
        let value = next_value.clone();
        next_value = (comb_fn)(value.clone());

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

        assert_eq!(s.iter().take(5).collect::<Vec<_>>(), [0, 4, 3, 1, 2]);
    }

    #[test]
    fn test_reg() {
        let clk = Clock::<TestSystem>::default();
        let r = reg::<TestSystem, Unsigned<3>>(clk, 0.into(), |val| val + 1);

        assert_eq!(r.iter().take(16).collect::<Vec<_>>(), [
            0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7
        ]);
    }
}
