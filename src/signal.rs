use std::{
    cell::{Cell, RefCell},
    fmt::Debug,
    marker::PhantomData,
    ops::{Add, BitAnd, BitOr, Not, Sub},
    rc::Rc,
};

use derive_where::derive_where;
use seq_macro::seq;

use crate::{
    bit::{Bit, H, L},
    cast::CastInner,
    domain::{Clock, ClockDomain},
    signal_fn::SignalFn,
    simulation::{SimCtx, Simulate, Watcher},
};

pub trait SignalValue: Debug + Copy + 'static {}

impl SignalValue for bool {}

macro_rules! impl_signal_value_for_tuples {
    (
        $n: literal
    ) => {
        seq!(N in 0..$n {
            impl< #( T~N: SignalValue, )* > SignalValue for ( #( T~N, )* ) {}

        });
     };
}

impl_signal_value_for_tuples!(1);
impl_signal_value_for_tuples!(2);
impl_signal_value_for_tuples!(3);
impl_signal_value_for_tuples!(4);
impl_signal_value_for_tuples!(5);
impl_signal_value_for_tuples!(6);
impl_signal_value_for_tuples!(7);
impl_signal_value_for_tuples!(8);
impl_signal_value_for_tuples!(9);
impl_signal_value_for_tuples!(10);
impl_signal_value_for_tuples!(11);
impl_signal_value_for_tuples!(12);

#[derive_where(Debug, Clone)]
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

    pub fn watch(mut self, name: &'static str) -> Self {
        self.name = Some(name);
        self
    }

    pub fn lift(value: T) -> Signal<D, T> {
        Self::new(move |_| value)
    }

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

    pub fn and_then<U: SignalValue, F>(self, f: F) -> Signal<D, U>
    where
        F: Fn(Wrapped<D, T>) -> Signal<D, U> + Clone + 'static,
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
}

impl<D: ClockDomain> Signal<D, Bit> {
    pub fn click(source: &Source<Bit>, f: impl FnOnce()) {
        source.revert();
        f();
        source.revert();
    }
}

#[allow(type_alias_bounds)]
pub type Reset<D: ClockDomain> = Signal<D, Bit>;

impl<D: ClockDomain> Reset<D> {
    pub fn reset() -> Self {
        Self::lift(L)
    }

    pub fn reset_src() -> (Source<Bit>, Self) {
        Self::source(L)
    }
}

#[allow(type_alias_bounds)]
pub type Enable<D: ClockDomain> = Signal<D, Bit>;

impl<D: ClockDomain> Enable<D> {
    pub fn enable() -> Self {
        Self::lift(H)
    }

    pub fn enable_src() -> (Source<Bit>, Self) {
        Self::source(H)
    }
}

impl<D: ClockDomain, T: SignalValue> Simulate for Signal<D, T> {
    type Value = T;

    fn next(&mut self, ctx: &mut SimCtx) -> Self::Value {
        self.next(ctx)
    }
}

#[derive_where(Debug)]
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

    pub(crate) fn next(&mut self, ctx: &mut SimCtx) {
        self.0.next(ctx);
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

    Signal::new(move |ctx| {
        let s1 = s1.next(ctx);
        let s2 = s2.next(ctx);
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

pub fn reg<D: ClockDomain, T: SignalValue>(
    _clock: Clock<D>,
    mut rst: Reset<D>,
    rst_val: T,
    comb_fn: impl Fn(T, Watcher<'_>) -> T + Clone + 'static,
) -> Signal<D, T> {
    let mut next_val = rst_val;
    Signal::new(move |ctx| {
        let value = if rst.next(ctx).into() {
            rst_val
        } else {
            next_val
        };
        next_val = (comb_fn)(value, ctx.watcher());

        value
    })
}

pub fn reg_en<D: ClockDomain, T: SignalValue>(
    _clock: Clock<D>,
    mut rst: Reset<D>,
    mut en: Enable<D>,
    rst_val: T,
    comb_fn: impl Fn(T, Watcher<'_>) -> T + Clone + 'static,
) -> Signal<D, T> {
    let mut next_val = rst_val;
    Signal::new(move |ctx| {
        if rst.next(ctx).into() {
            next_val = rst_val;
            (comb_fn)(next_val, ctx.watcher());
            next_val
        } else if en.next(ctx).into() {
            let val = (comb_fn)(next_val, ctx.watcher());
            next_val = val;
            val
        } else {
            (comb_fn)(next_val, ctx.watcher());
            next_val
        }
    })
}

pub trait Unbundle {
    type Unbundled;

    fn unbundle(self) -> Self::Unbundled;
}

pub trait Bundle {
    type Bundled: Unbundle<Unbundled = Self>;
    fn bundle(self) -> Self::Bundled;
}

macro_rules! impl_bundle_for_tuples {
    (
        $n:literal
    ) => {
        seq!(N in 0..$n {
            impl<D: ClockDomain, #( T~N: SignalValue, )*> Unbundle for Signal<D, ( #(T~N,)* )> {
                type Unbundled = ( #( Signal<D, T~N>, )* );

                fn unbundle(self) -> Self::Unbundled {
                    (
                        #(
                            self.clone().map(|values| values.N),
                        )*

                    )
                }
            }

            impl<D: ClockDomain, #( T~N: SignalValue, )*> Bundle for ( #( Signal<D, T~N>, )* ) {
                type Bundled = Signal<D, ( #(T~N,)* )>;
                fn bundle(mut self) -> Self::Bundled {
                    Signal::new(move |ctx| (
                        #(
                            self.N.next(ctx),
                        )*
                    ))
                }
            }


            impl<D: ClockDomain, #( T~N: SignalValue, )*> Simulate for ( #( Signal<D, T~N>, )* ) {
                type Value = ( #( T~N, )* );

                fn next(&mut self, ctx: &mut SimCtx) -> Self::Value {
                    (
                        #(
                            self.N.next(ctx),
                        )*
                    )
                }
            }

            impl<#( U~N, T~N: CastInner<U~N>, )*> CastInner<( #( U~N, )* )> for ( #( T~N, )* ) {
                fn cast_inner(self) -> ( #( U~N, )* ) {
                    (
                        #(
                            self.N.cast_inner(),
                        )*
                    )
                }
            }
        });


    };
}

impl_bundle_for_tuples!(1);
impl_bundle_for_tuples!(2);
impl_bundle_for_tuples!(3);
impl_bundle_for_tuples!(4);
impl_bundle_for_tuples!(5);
impl_bundle_for_tuples!(6);
impl_bundle_for_tuples!(7);
impl_bundle_for_tuples!(8);
impl_bundle_for_tuples!(9);
impl_bundle_for_tuples!(10);
impl_bundle_for_tuples!(11);
impl_bundle_for_tuples!(12);

#[cfg(test)]
mod tests {
    use super::{SignalIterExt, *};
    use crate::{cast::Cast, unsigned::Unsigned};

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

        assert_eq!(s.simulate().take(5).collect::<Vec<_>>(), [0, 4, 3, 1, 2]);
    }

    #[test]
    fn test_reg() {
        let clk = Clock::<TestSystem>::default();
        let rst = Reset::reset();
        let r = reg::<TestSystem, Unsigned<3>>(clk, rst, 0.into(), |val, _| val + 1);

        assert_eq!(r.simulate().take(16).collect::<Vec<_>>(), [
            0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7
        ]);
    }

    #[test]
    fn unbundle() {
        let s: Signal<TestSystem, (Unsigned<4>, Bit)> = [
            (0, false),
            (1, true),
            (2, true),
            (3, false),
            (4, true),
            (5, false),
        ]
        .into_iter()
        .map(Cast::cast)
        .into_signal();

        let res = s.unbundle();

        assert_eq!(
            res.simulate().take(6).map(Cast::cast).collect::<Vec<_>>(),
            [
                (0, false),
                (1, true),
                (2, true),
                (3, false),
                (4, true),
                (5, false),
            ]
        );
    }

    #[test]
    fn bundle() {
        let s: (Signal<TestSystem, Unsigned<4>>, Signal<TestSystem, Bit>) = (
            [0, 1, 2, 3, 4, 5].into_iter().map(Cast::cast).into_signal(),
            [false, true, true, false, true, false]
                .into_iter()
                .map(Cast::cast)
                .into_signal(),
        );

        let res = s.bundle();

        assert_eq!(
            res.simulate().take(6).map(Cast::cast).collect::<Vec<_>>(),
            [
                (0, false),
                (1, true),
                (2, true),
                (3, false),
                (4, true),
                (5, false),
            ]
        );
    }
}
