use std::{
    borrow::{Borrow, Cow},
    cell::RefCell,
    fmt::Debug,
    marker::PhantomData,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Sub},
    rc::Rc,
};

use derive_where::derive_where;
pub use fhdl_macros::SignalValue;
use fhdl_macros::{blackbox, blackbox_ty, synth};

use crate::{
    bit::Bit,
    cast::{Cast, CastFrom},
    domain::{Clock, ClockDomain},
    signal_fn::SignalFn,
    simulation::{SimCtx, Simulate},
    watchable::{AsDisplay, FmtKind, Formatter, Watchable},
};

pub trait SignalValue: Debug + Clone + 'static {}

impl SignalValue for bool {}

impl<T: SignalValue> SignalValue for Option<T> {}

#[derive_where(Debug, Clone)]
#[blackbox_ty(Signal)]
pub struct Signal<D: ClockDomain, T: SignalValue> {
    #[derive_where(skip)]
    _dom: PhantomData<D>,
    next: Rc<RefCell<SignalFn<T>>>,
    fmt: Option<Formatter<T>>,
}

impl<D: ClockDomain, T: SignalValue> Signal<D, T> {
    pub(crate) fn new(f: impl FnMut(&mut SimCtx) -> T + 'static) -> Self {
        Self {
            _dom: PhantomData,
            next: Rc::new(RefCell::new(SignalFn::new(f))),
            fmt: None,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub(crate) fn next(&mut self, ctx: &mut SimCtx) -> T {
        let value = self.next.borrow_mut().next_val(ctx);
        if let Some(fmt) = self.fmt.as_ref() {
            ctx.watch(&value, fmt);
        }

        value
    }

    #[blackbox(SignalWatch)]
    pub fn watch(self, name: impl Into<Cow<'static, str>>) -> Self
    where
        T: Watchable<AsDisplay>,
    {
        self.watch_with(name)
    }

    #[blackbox(SignalWatch)]
    pub fn watch_with<F: FmtKind, I: Into<Cow<'static, str>>>(mut self, name: I) -> Self
    where
        T: Watchable<F>,
    {
        self.fmt = Some(T::formatter(name));
        self
    }

    #[blackbox(SignalLift)]
    pub fn lift(value: T) -> Signal<D, T> {
        Self::new(move |_| value.clone())
    }

    #[blackbox(SignalMap)]
    pub fn map<U: SignalValue, F>(&self, f: F) -> Signal<D, U>
    where
        F: Fn(T) -> U + Clone + 'static,
    {
        let mut inner = self.clone();
        Signal::new(move |ctx| {
            let val = inner.next(ctx);
            (f)(val)
        })
    }

    #[blackbox(SignalAndThen)]
    pub fn and_then<U: SignalValue, F>(&self, f: F) -> Signal<D, U>
    where
        F: FnOnce(Wrapped<D, T>) -> Signal<D, U> + Clone,
    {
        let mut wrapped = Wrapped::new(self.clone());
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
        &self,
        other: impl Into<Signal<D, U>>,
        f: F,
    ) -> Signal<D, V>
    where
        F: Fn(T, U) -> V + Clone + 'static,
    {
        let mut this = self.clone();
        let mut other = other.into();

        Signal::new(move |ctx| {
            let this = this.next(ctx);
            let other = other.next(ctx);
            (f)(this, other)
        })
    }

    #[blackbox(SignalEq)]
    pub fn eq<U: SignalValue>(&self, other: impl Into<Signal<D, U>>) -> Signal<D, bool>
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
    #[synth]
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
    #[synth]
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
    #[inline]
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
    #[inline]
    #[blackbox(SignalValue)]
    pub fn value(&self) -> T {
        RefCell::borrow(&self.0.next).value()
    }

    pub(crate) fn next(&mut self, ctx: &mut SimCtx) {
        self.0.next(ctx);
    }

    #[blackbox(SignalWatch)]
    pub fn watch(self, name: &'static str) -> Self
    where
        T: Watchable<AsDisplay>,
    {
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
    rst: &Reset<D>,
    rst_val: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let mut rst = rst.clone();
    let rst_val = rst_val.borrow().clone();

    let mut next_val = rst_val.clone();
    Signal::new(move |ctx| {
        if rst.next(ctx) {
            // Asynchronous reset
            next_val = rst_val.clone();
            next_val.clone()
        } else {
            let val = next_val.clone();
            next_val = (comb_fn)(val.clone());
            val
        }
    })
}

#[blackbox(SignalRegEn)]
pub fn reg_en<D: ClockDomain, T: SignalValue>(
    _clock: Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    rst_val: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let mut rst = rst.clone();
    let mut en = en.clone();
    let rst_val = rst_val.borrow().clone();

    let mut next_val = rst_val.clone();
    Signal::new(move |ctx| {
        if rst.next(ctx) {
            // Asynchronous reset
            next_val = rst_val.clone();
            next_val.clone()
        } else if en.next(ctx) {
            let val = next_val.clone();
            next_val = (comb_fn)(val.clone());
            val
        } else {
            let val = next_val.clone();
            (comb_fn)(val.clone());
            val
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
    use std::iter;

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
        let (rst, rst_signal) = Reset::reset_src();

        let mut r =
            reg::<TestSystem4, Unsigned<3>>(clk, &rst_signal, &0_u8.cast(), |val| {
                val + 1_u8
            })
            .simulate();

        assert_eq!(r.by_ref().take(15).collect::<Vec<_>>(), [
            0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6
        ]);

        rst.revert();
        assert_eq!(r.by_ref().take(5).collect::<Vec<_>>(), [0, 0, 0, 0, 0]);

        rst.revert();
        assert_eq!(r.by_ref().take(15).collect::<Vec<_>>(), [
            0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6
        ]);
    }

    #[test]
    fn test_reg_en() {
        let clk = Clock::<TestSystem4>::default();
        let (rst, rst_signal) = Reset::reset_src();
        let (en, en_signal) = Enable::enable_src();

        let mut r =
            reg_en::<_, Unsigned<3>>(clk, &rst_signal, &en_signal, &0_u8.cast(), |val| {
                val + 1_u8
            })
            .simulate();

        assert_eq!(r.by_ref().take(15).collect::<Vec<_>>(), [
            0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6
        ]);

        rst.revert();
        assert_eq!(r.by_ref().take(5).collect::<Vec<_>>(), [0, 0, 0, 0, 0]);

        rst.revert();
        assert_eq!(r.by_ref().take(14).collect::<Vec<_>>(), [
            0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5
        ]);

        en.revert();
        assert_eq!(r.by_ref().take(15).collect::<Vec<_>>(), [6; 15]);

        en.revert();
        assert_eq!(r.by_ref().take(15).collect::<Vec<_>>(), [
            6, 7, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4
        ]);
    }

    #[test]
    fn test_reg_seq() {
        let clk = Clock::<TestSystem4>::default();
        let (rst, rst_signal) = Reset::reset_src();
        let (en, en_signal) = Enable::enable_src();

        let data = iter::successors::<Unsigned<3>, _>(Some(1_u8.cast()), |val| {
            Some(val.clone() + 1_u8)
        })
        .into_signal::<TestSystem4>();

        macro_rules! block {
            ($pred:ident) => {
                $pred.and_then(|data| {
                    reg_en(
                        clk,
                        &rst_signal,
                        &en_signal,
                        &0_u8.cast::<Unsigned<3>>(),
                        move |_| data.value(),
                    )
                })
            };
        }

        let reg0 = block!(data);
        let reg1 = block!(reg0);
        let reg2 = block!(reg1);
        let reg3 = block!(reg2);

        let r = [reg0, reg1, reg2, reg3].bundle();
        let mut r = (data, en_signal, r).bundle().simulate();

        let cast = |value: (Unsigned<3>, bool, [Unsigned<3>; 4])| {
            value.cast::<(u8, bool, [u8; 4])>()
        };

        assert_eq!(r.by_ref().take(4).map(cast).collect::<Vec<_>>(), [
            (1, true, [0, 0, 0, 0]),
            (2, true, [1, 0, 0, 0]),
            (3, true, [2, 1, 0, 0]),
            (4, true, [3, 2, 1, 0])
        ]);

        en.revert();
        assert_eq!(r.by_ref().take(4).map(cast).collect::<Vec<_>>(), [
            (5, false, [4, 3, 2, 1]),
            (6, false, [4, 3, 2, 1]),
            (7, false, [4, 3, 2, 1]),
            (0, false, [4, 3, 2, 1])
        ]);

        en.revert();
        assert_eq!(r.by_ref().take(4).map(cast).collect::<Vec<_>>(), [
            (1, true, [4, 3, 2, 1]),
            (2, true, [1, 4, 3, 2]),
            (3, true, [2, 1, 4, 3]),
            (4, true, [3, 2, 1, 4]),
        ]);

        rst.revert();
        assert_eq!(cast(r.next_cycle()), (5, true, [0, 0, 0, 0]));
        assert_eq!(cast(r.next_cycle()), (6, true, [0, 0, 0, 0]));
        rst.revert();

        assert_eq!(r.by_ref().take(4).map(cast).collect::<Vec<_>>(), [
            (7, true, [0, 0, 0, 0]),
            (0, true, [7, 0, 0, 0]),
            (1, true, [0, 7, 0, 0]),
            (2, true, [1, 0, 7, 0]),
        ]);
    }
}
