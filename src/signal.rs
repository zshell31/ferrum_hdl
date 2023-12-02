use std::{
    cell::RefCell,
    fmt::Debug,
    marker::PhantomData,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Sub},
    rc::Rc,
};

use derive_where::derive_where;
pub use fhdl_macros::SignalValue;
use fhdl_macros::{blackbox, blackbox_ty};

use crate::{
    bit::Bit,
    cast::{Cast, CastFrom},
    domain::{Clock, ClockDomain},
    signal_fn::SignalFn,
    simulation::{SimCtx, Simulate},
};

pub trait SignalValue: Debug + Clone + 'static {}

impl SignalValue for bool {}

#[derive_where(Debug, Clone)]
#[blackbox_ty(Signal)]
pub struct Signal<D: ClockDomain, T: SignalValue> {
    #[derive_where(skip)]
    _dom: PhantomData<D>,
    next: Rc<RefCell<SignalFn<T>>>,
    name: Option<&'static str>,
}

impl<D: ClockDomain, T: SignalValue> Signal<D, T> {
    pub(crate) fn new(f: impl FnMut(&mut SimCtx) -> T + 'static) -> Self {
        Self {
            _dom: PhantomData,
            next: Rc::new(RefCell::new(SignalFn::new(f))),
            name: None,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub(crate) fn next(&mut self, ctx: &mut SimCtx) -> T {
        let value = self.next.borrow_mut().next_val(ctx);
        if let Some(name) = self.name.as_ref() {
            ctx.watch(name, &value);
        }

        value
    }

    #[blackbox(SignalWatch)]
    pub fn watch(mut self, name: &'static str) -> Self {
        self.name = Some(name);
        self
    }

    #[blackbox(SignalLift)]
    pub fn lift(value: T) -> Signal<D, T> {
        Self::new(move |_| value.clone())
    }

    #[blackbox(SignalMap)]
    pub fn map<U: SignalValue, F>(self, f: F) -> Signal<D, U>
    where
        F: Fn(T) -> U + Clone + 'static,
    {
        let mut inner = self;
        Signal::new(move |ctx| {
            let val = inner.next(ctx);
            (f)(val)
        })
    }

    #[blackbox(SignalAndThen)]
    pub fn and_then<U: SignalValue, F>(self, f: F) -> Signal<D, U>
    where
        F: FnOnce(Wrapped<D, T>) -> Signal<D, U> + Clone + 'static,
    {
        let mut wrapped = Wrapped::new(self);
        let mut signal = f(wrapped.clone());
        Signal::new(move |ctx| {
            wrapped.next(ctx);
            signal.next(ctx)
        })
    }

    pub fn source(value: T) -> (Source<T>, Signal<D, T>) {
        let source = Source::new(value);
        let source_clone = source.clone();
        let signal = Signal::new(move |_| source_clone.value());

        (source, signal)
    }

    #[blackbox(SignalApply2)]
    pub fn apply2<U: SignalValue, V: SignalValue, F>(
        self,
        other: impl Into<Signal<D, U>>,
        f: F,
    ) -> Signal<D, V>
    where
        F: Fn(T, U) -> V + Clone + 'static,
    {
        let mut this = self;
        let mut other = other.into();

        Signal::new(move |ctx| {
            let this = this.next(ctx);
            let other = other.next(ctx);
            (f)(this, other)
        })
    }

    #[blackbox(SignalEq)]
    pub fn eq<U: SignalValue>(self, other: impl Into<Signal<D, U>>) -> Signal<D, bool>
    where
        T: PartialEq<U>,
    {
        self.apply2(other, |this, other| this == other)
    }
}

impl<D: ClockDomain> Signal<D, bool> {
    pub fn click(source: &Source<bool>, f: impl FnOnce()) {
        source.revert();
        f();
        source.revert();
    }

    #[blackbox(SignalAnd)]
    pub fn and(self, other: impl Into<Self>) -> Self {
        self.apply2(other, |this, other| this && other)
    }

    #[blackbox(SignalOr)]
    pub fn or(self, other: impl Into<Self>) -> Self {
        self.apply2(other, |this, other| this || other)
    }
}

impl<D: ClockDomain> Signal<D, Bit> {
    pub fn click(source: &Source<Bit>, f: impl FnOnce()) {
        source.revert();
        f();
        source.revert();
    }

    #[blackbox(SignalAnd)]
    pub fn and(self, other: impl Into<Self>) -> Self {
        self.apply2(other, |this, other| {
            Bit::cast_from(this.cast() && other.cast())
        })
    }

    #[blackbox(SignalOr)]
    pub fn or(self, other: impl Into<Self>) -> Self {
        self.apply2(other, |this, other| {
            Bit::cast_from(this.cast() || other.cast())
        })
    }
}

impl<T: SignalValue, D: ClockDomain> From<T> for Signal<D, T> {
    fn from(value: T) -> Self {
        Self::lift(value)
    }
}

#[allow(type_alias_bounds)]
pub type Reset<D: ClockDomain> = Signal<D, bool>;

impl<D: ClockDomain> Reset<D> {
    #[blackbox(SignalReset)]
    pub fn reset() -> Self {
        Self::lift(false)
    }

    pub fn reset_src() -> (Source<bool>, Self) {
        Self::source(false)
    }
}

#[allow(type_alias_bounds)]
pub type Enable<D: ClockDomain> = Signal<D, bool>;

impl<D: ClockDomain> Enable<D> {
    pub fn enable() -> Self {
        Self::lift(true)
    }

    pub fn enable_src() -> (Source<bool>, Self) {
        Self::source(true)
    }
}

impl<D: ClockDomain, T: SignalValue> Simulate for Signal<D, T> {
    type Value = T;

    fn next(&mut self, ctx: &mut SimCtx) -> Self::Value {
        self.next(ctx)
    }
}

#[derive_where(Debug)]
#[blackbox_ty(Wrapped)]
pub struct Wrapped<D: ClockDomain, T: SignalValue>(Signal<D, T>);

impl<D: ClockDomain, T: SignalValue> Clone for Wrapped<D, T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<D: ClockDomain, T: SignalValue> From<Wrapped<D, T>> for Signal<D, T> {
    fn from(wrapped: Wrapped<D, T>) -> Self {
        wrapped.0
    }
}

impl<D: ClockDomain, T: SignalValue> Wrapped<D, T> {
    fn new(signal: Signal<D, T>) -> Self {
        Self(signal)
    }

    #[allow(clippy::should_implement_trait)]
    #[inline(always)]
    #[blackbox(SignalValue)]
    pub fn value(&self) -> T {
        self.0.next.borrow().value()
    }

    pub(crate) fn next(&mut self, ctx: &mut SimCtx) {
        self.0.next(ctx);
    }

    #[blackbox(SignalWatch)]
    pub fn watch(self, name: &'static str) -> Self {
        Self(self.0.watch(name))
    }
}

impl<D: ClockDomain> Wrapped<D, bool> {
    #[blackbox(SignalAnd)]
    pub fn and(self, other: impl Into<Signal<D, bool>>) -> Signal<D, bool> {
        Signal::from(self).and(other)
    }

    #[blackbox(SignalOr)]
    pub fn or(self, other: impl Into<Signal<D, bool>>) -> Signal<D, bool> {
        Signal::from(self).or(other)
    }
}

impl<D: ClockDomain> Wrapped<D, Bit> {
    #[blackbox(SignalAnd)]
    pub fn and(self, other: impl Into<Signal<D, Bit>>) -> Signal<D, Bit> {
        Signal::from(self).and(other)
    }

    #[blackbox(SignalOr)]
    pub fn or(self, other: impl Into<Signal<D, Bit>>) -> Signal<D, Bit> {
        Signal::from(self).or(other)
    }
}

#[derive(Debug, Clone)]
pub struct Source<T: SignalValue>(Rc<RefCell<T>>);

impl<T: SignalValue> Source<T> {
    pub(crate) fn new(value: T) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }

    pub fn value(&self) -> T {
        self.0.borrow().clone()
    }

    pub fn set_value(&self, value: T) -> T {
        self.0.replace(value)
    }
}

impl Source<Bit> {
    pub fn revert(&self) {
        self.0.replace_with(|val| !(*val));
    }
}

impl Source<bool> {
    pub fn revert(&self) {
        self.0.replace_with(|val| !(*val));
    }
}

impl<D: ClockDomain, T: SignalValue, U: SignalValue> BitAnd<Signal<D, U>> for Signal<D, T>
where
    T: BitAnd<U>,
    <T as BitAnd<U>>::Output: SignalValue,
{
    type Output = Signal<D, <T as BitAnd<U>>::Output>;

    fn bitand(self, rhs: Signal<D, U>) -> Self::Output {
        self.apply2(rhs, |lhs, rhs| lhs.bitand(rhs))
    }
}

impl<D: ClockDomain, T: SignalValue, U: SignalValue> BitOr<Signal<D, U>> for Signal<D, T>
where
    T: BitOr<U>,
    <T as BitOr<U>>::Output: SignalValue,
{
    type Output = Signal<D, <T as BitOr<U>>::Output>;

    fn bitor(self, rhs: Signal<D, U>) -> Self::Output {
        self.apply2(rhs, |lhs, rhs| lhs.bitor(rhs))
    }
}

impl<D: ClockDomain, T: SignalValue, U: SignalValue> BitXor<Signal<D, U>> for Signal<D, T>
where
    T: BitXor<U>,
    <T as BitXor<U>>::Output: SignalValue,
{
    type Output = Signal<D, <T as BitXor<U>>::Output>;

    fn bitxor(self, rhs: Signal<D, U>) -> Self::Output {
        self.apply2(rhs, |lhs, rhs| lhs.bitxor(rhs))
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
        self.apply2(rhs, |lhs, rhs| lhs.add(rhs))
    }
}

impl<D: ClockDomain, T: SignalValue, U: SignalValue> Sub<Signal<D, U>> for Signal<D, T>
where
    T: Sub<U>,
    <T as Sub<U>>::Output: SignalValue,
{
    type Output = Signal<D, <T as Sub<U>>::Output>;

    fn sub(self, rhs: Signal<D, U>) -> Self::Output {
        self.apply2(rhs, |lhs, rhs| lhs.sub(rhs))
    }
}

impl<D: ClockDomain, T: SignalValue, U: SignalValue> Mul<Signal<D, U>> for Signal<D, T>
where
    T: Mul<U>,
    <T as Mul<U>>::Output: SignalValue,
{
    type Output = Signal<D, <T as Mul<U>>::Output>;

    fn mul(self, rhs: Signal<D, U>) -> Self::Output {
        self.apply2(rhs, |lhs, rhs| lhs.mul(rhs))
    }
}

impl<D: ClockDomain, T: SignalValue, U: SignalValue> Div<Signal<D, U>> for Signal<D, T>
where
    T: Div<U>,
    <T as Div<U>>::Output: SignalValue,
{
    type Output = Signal<D, <T as Div<U>>::Output>;

    fn div(self, rhs: Signal<D, U>) -> Self::Output {
        self.apply2(rhs, |lhs, rhs| lhs.div(rhs))
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

#[blackbox(SignalReg)]
pub fn reg<D: ClockDomain, T: SignalValue>(
    _clock: Clock<D>,
    mut rst: Reset<D>,
    rst_val: T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let mut next_val = rst_val.clone();
    Signal::new(move |ctx| {
        let value = if rst.next(ctx) {
            rst_val.clone()
        } else {
            next_val.clone()
        };
        next_val = (comb_fn)(value.clone());

        value
    })
}

#[blackbox(SignalRegEn)]
pub fn reg_en<D: ClockDomain, T: SignalValue>(
    _clock: Clock<D>,
    mut rst: Reset<D>,
    mut en: Signal<D, impl Into<bool> + SignalValue>,
    rst_val: T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let mut next_val = rst_val.clone();
    Signal::new(move |ctx| {
        if rst.next(ctx) {
            next_val = rst_val.clone();
            (comb_fn)(next_val.clone());
            next_val.clone()
        } else if en.next(ctx).into() {
            let val = (comb_fn)(next_val.clone());
            next_val = val.clone();
            val
        } else {
            (comb_fn)(next_val.clone());
            next_val.clone()
        }
    })
}

pub trait Unbundle {
    type Unbundled;

    #[blackbox(Unbundle)]
    fn unbundle(self) -> Self::Unbundled;
}

pub trait Bundle {
    type Bundled: Unbundle<Unbundled = Self>;

    #[blackbox(Bundle)]
    fn bundle(self) -> Self::Bundled;
}

#[cfg(test)]
mod tests {
    use super::{SignalIterExt, *};
    use crate::{domain::TestSystem4, unsigned::Unsigned};

    #[test]
    fn test_iter() {
        let s = [0_u8, 4, 3, 1, 2]
            .into_iter()
            .map(Unsigned::<8>::cast_from)
            .into_signal::<TestSystem4>();

        assert_eq!(s.simulate().take(5).collect::<Vec<_>>(), [0, 4, 3, 1, 2]);
    }

    #[test]
    fn test_reg() {
        let clk = Clock::<TestSystem4>::default();
        let rst = Reset::reset();
        let r = reg::<TestSystem4, Unsigned<3>>(clk, rst, 0_u8.cast(), |val| val + 1_u8);

        assert_eq!(r.simulate().take(16).collect::<Vec<_>>(), [
            0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7
        ]);
    }
}
