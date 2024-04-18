use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub};

use fhdl_macros::{blackbox, synth};

use super::{Signal, SignalValue, Wrapped};
use crate::{domain::ClockDomain, prelude::Bit};

pub trait IntoSignal<D: ClockDomain> {
    type Value: SignalValue;

    fn into_signal(self) -> Signal<D, Self::Value>;
}

impl<T: SignalValue, D: ClockDomain> IntoSignal<D> for Signal<D, T> {
    type Value = T;

    #[blackbox(IntoSignal)]
    #[inline]
    fn into_signal(self) -> Signal<D, Self::Value> {
        self
    }
}

impl<T: SignalValue, D: ClockDomain> IntoSignal<D> for &'_ Signal<D, T> {
    type Value = T;

    #[blackbox(IntoSignal)]
    #[inline]
    fn into_signal(self) -> Signal<D, Self::Value> {
        self.clone()
    }
}

impl<T: SignalValue, D: ClockDomain> IntoSignal<D> for T {
    type Value = T;

    #[blackbox(IntoSignal)]
    #[inline]
    fn into_signal(self) -> Signal<D, Self::Value> {
        Signal::lift(self)
    }
}

macro_rules! impl_cmp {
    ($( $method:ident => $bound:ident, )* $(,)?) => {
        $(
            #[synth(inline)]
            #[inline]
            pub fn $method<U: IntoSignal<D>>(&self, other: U) -> Signal<D, Bit>
            where
                T: $bound<U::Value>
            {
                self.apply2(other, |lhs, rhs| lhs.$method(&rhs))
            }
        )*
    };
}

impl<D: ClockDomain, T: SignalValue> Signal<D, T> {
    #[blackbox(SignalApply2)]
    pub fn apply2<U: IntoSignal<D>, V: SignalValue, F>(
        &self,
        other: U,
        f: F,
    ) -> Signal<D, V>
    where
        F: Fn(T, U::Value) -> V + Clone + 'static,
    {
        let mut this = self.clone();
        let mut other = other.into_signal();

        Signal::new(move |ctx| {
            let this = this.next(ctx);
            let other = other.next(ctx);
            (f)(this, other)
        })
    }

    impl_cmp!(
        eq => PartialEq,
        ne => PartialEq,
        lt => PartialOrd,
        le => PartialOrd,
        gt => PartialOrd,
        ge => PartialOrd,
    );
}

impl<D: ClockDomain> Signal<D, Bit> {
    #[synth(inline)]
    pub fn and<U: IntoSignal<D, Value = Bit>>(&self, other: U) -> Signal<D, bool> {
        self.apply2(other, |lhs, rhs| lhs && rhs)
    }

    #[synth(inline)]
    pub fn or<U: IntoSignal<D, Value = Bit>>(&self, other: U) -> Signal<D, bool> {
        self.apply2(other, |lhs, rhs| lhs || rhs)
    }
}

macro_rules! bin_op {
    ( impl $imp:ident $(( $($gen:tt)* ))? with $method:ident for $t:ty, $u:ty ) => {
        impl<'a, D: ClockDomain, $( $($gen)* )? > $imp<$u> for $t
        where
            $t: IntoSignal<D>,
            $u: IntoSignal<D>,
            <$t as IntoSignal<D>>::Value: $imp<<$u as IntoSignal<D>>::Value>,
            < <$t as IntoSignal<D>>::Value as $imp< <$u as IntoSignal<D>>::Value> >::Output: SignalValue
        {
            type Output = Signal<D, < <$t as IntoSignal<D>>::Value as $imp< <$u as IntoSignal<D>>::Value> >::Output>;

            #[synth(inline)]
            fn $method(self, rhs: $u) -> Self::Output {
                self.into_signal().apply2(rhs, |lhs, rhs| lhs.$method(rhs))
            }
        }
    };
}

macro_rules! impl_bin_op {
    ( $( $imp:ident => $method:ident ),+ $(,)? ) => {
        $(
            bin_op!(impl $imp ( T: SignalValue, U: SignalValue ) with $method for Signal<D, T>, U );
            bin_op!(impl $imp ( T: SignalValue, U: SignalValue ) with $method for Signal<D, T>, Signal<D, U> );
            bin_op!(impl $imp ( T: SignalValue, U: SignalValue ) with $method for Signal<D, T>, &'a Signal<D, U> );
            bin_op!(impl $imp ( T: SignalValue, U: SignalValue ) with $method for &'a Signal<D, T>, Signal<D, U> );
            bin_op!(impl $imp ( T: SignalValue, U: SignalValue ) with $method for &'a Signal<D, T>, &'a Signal<D, U> );

            bin_op!(impl $imp ( T: SignalValue, U: SignalValue ) with $method for Wrapped<D, T>, U );
            bin_op!(impl $imp ( T: SignalValue, U: SignalValue ) with $method for Wrapped<D, T>, Wrapped<D, U> );
            bin_op!(impl $imp ( T: SignalValue, U: SignalValue ) with $method for Wrapped<D, T>, Signal<D, U> );
            bin_op!(impl $imp ( T: SignalValue, U: SignalValue ) with $method for Signal<D, T>, Wrapped<D, U> );
            bin_op!(impl $imp ( T: SignalValue, U: SignalValue ) with $method for Wrapped<D, T>, &'a Signal<D, U> );
            bin_op!(impl $imp ( T: SignalValue, U: SignalValue ) with $method for &'a Signal<D, T>, Wrapped<D, U> );
        )+
    };
}

impl_bin_op!(
    Add => add,
    Sub => sub,
    Mul => mul,
    Div => div,
    Rem => rem,
    Shl => shl,
    Shr => shr,
    BitAnd => bitand,
    BitOr => bitor,
    BitXor => bitxor,
);

impl<D: ClockDomain, T: SignalValue> Not for Signal<D, T>
where
    T: Not,
    <T as Not>::Output: SignalValue,
{
    type Output = Signal<D, <T as Not>::Output>;

    #[synth(inline)]
    fn not(self) -> Self::Output {
        self.map(|val| val.not())
    }
}

impl<D: ClockDomain, T: SignalValue> Not for &'_ Signal<D, T>
where
    T: Not,
    <T as Not>::Output: SignalValue,
{
    type Output = Signal<D, <T as Not>::Output>;

    #[synth(inline)]
    fn not(self) -> Self::Output {
        self.map(|val| val.not())
    }
}

impl<D: ClockDomain, T: SignalValue> Not for Wrapped<D, T>
where
    T: Not,
    <T as Not>::Output: SignalValue,
{
    type Output = Signal<D, <T as Not>::Output>;

    #[synth(inline)]
    fn not(self) -> Self::Output {
        self.into_signal().map(|val| val.not())
    }
}
