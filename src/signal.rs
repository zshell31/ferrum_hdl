mod counters;
mod ops;
mod reg;
mod signal_fn;
mod wrapped;

use std::{
    cell::RefCell,
    fmt::{self, Display},
    io,
    marker::PhantomData,
    rc::Rc,
};

pub use counters::{rise_every, rise_period, rise_rate};
use derive_where::derive_where;
pub use fhdl_macros::SignalValue;
use fhdl_macros::{blackbox, blackbox_ty, synth};
pub use ops::IntoSignal;
pub use reg::{
    dff, dff_comb, reg, reg0, reg0_comb, reg_comb, reg_en, reg_en0, reg_en0_comb,
    reg_en_comb, Enable, Reset,
};
use vcd::IdCode;
pub use wrapped::Wrapped;

use self::signal_fn::SignalFn;
use crate::{
    bit::Bit,
    domain::{Clock, ClockDomain},
    eval::{Eval, EvalCtx},
    prelude::Traceable,
    trace::{TraceVars, Tracer},
};

pub trait SignalValue: Clone + 'static {}

impl SignalValue for () {}

impl<T: SignalValue> SignalValue for Option<T> {}

impl<D: ClockDomain, T: SignalValue> Eval<D> for T {
    type Value = T;

    fn next(&mut self, _: &mut EvalCtx) -> Self::Value {
        self.clone()
    }
}

#[derive_where(Debug, Clone; T)]
#[blackbox_ty(Signal)]
pub struct Signal<D: ClockDomain, T: SignalValue> {
    #[derive_where(skip)]
    _dom: PhantomData<D>,
    next: Rc<RefCell<SignalFn<T>>>,
    value: Option<Rc<RefCell<T>>>,
}

impl<D: ClockDomain, T: SignalValue + Display> Display for Signal<D, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.next.borrow().as_value_opt() {
            Some(val) => Display::fmt(val, f),
            None => f.write_str("x"),
        }
    }
}

impl<D: ClockDomain, T: SignalValue> Signal<D, T> {
    pub(crate) fn new(f: impl FnMut(&mut EvalCtx) -> T + 'static) -> Self {
        Self {
            _dom: PhantomData,
            next: Rc::new(RefCell::new(SignalFn::new(f))),
            value: None,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub(crate) fn next(&mut self, ctx: &mut EvalCtx) -> T {
        self.next.borrow_mut().next_val(ctx)
    }

    #[blackbox(IntoSignal)]
    pub fn lift(value: T) -> Signal<D, T> {
        let value = Rc::new(RefCell::new(value));
        let value_clone = value.clone();
        let mut this = Self::new(move |_| value_clone.borrow().clone());
        this.value = Some(value);

        this
    }

    pub fn replace_value(&self, f: impl FnOnce(T) -> T) {
        if let Some(inner) = self.value.as_ref() {
            inner.replace_with(|val| f(val.clone()));
        }
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

    #[synth(inline)]
    pub fn reg<U: SignalValue + Default>(
        &self,
        clk: &Clock<D>,
        rst: &Reset<D>,
        f: impl Fn(T) -> U + Clone + 'static,
    ) -> Signal<D, U> {
        self.and_then(|value| reg0(clk, rst, move |_| f(value.value())))
    }

    #[synth(inline)]
    pub fn reg_en<U: SignalValue + Default>(
        &self,
        clk: &Clock<D>,
        rst: &Reset<D>,
        en: &Enable<D>,
        f: impl Fn(T) -> U + Clone + 'static,
    ) -> Signal<D, U> {
        self.and_then(|value| reg_en0(clk, rst, en, move |_| f(value.value())))
    }

    #[synth(inline)]
    pub fn into_reg(&self, clk: &Clock<D>, rst: &Reset<D>) -> Signal<D, T>
    where
        T: Default,
    {
        self.and_then(|value| reg0(clk, rst, move |_| value.value()))
    }
}

impl<D: ClockDomain> Signal<D, Bit> {
    pub fn invert(&self) {
        self.replace_value(|value| !value)
    }
}

impl<T: SignalValue, D: ClockDomain> From<T> for Signal<D, T> {
    #[synth(inline)]
    fn from(value: T) -> Self {
        Self::lift(value)
    }
}

impl<T: SignalValue, D: ClockDomain> From<&'_ Signal<D, T>> for Signal<D, T> {
    #[synth(inline)]
    fn from(signal: &'_ Signal<D, T>) -> Self {
        signal.clone()
    }
}

impl<T: SignalValue + Traceable, D: ClockDomain> Traceable for Signal<D, T> {
    #[inline]
    fn add_vars(vars: &mut TraceVars) {
        T::add_vars(vars)
    }

    fn trace(&self, id: &mut IdCode, tracer: &mut Tracer) -> io::Result<()> {
        if let Some(val) = self.next.borrow().as_value_opt() {
            val.trace(id, tracer)?;
        }

        Ok(())
    }
}

impl<D: ClockDomain, T: SignalValue> Eval<D> for Signal<D, T> {
    type Value = T;

    fn next(&mut self, ctx: &mut EvalCtx) -> Self::Value {
        self.next(ctx)
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

#[cfg(test)]
mod tests {
    use super::SignalIterExt;
    use crate::{
        cast::CastFrom,
        domain::{Clock, TD4},
        prelude::Eval,
        unsigned::U,
    };

    #[test]
    fn test_iter() {
        let clk = Clock::<TD4>::new();
        let s = [0_u8, 4, 3, 1, 2]
            .into_iter()
            .map(U::<8>::cast_from)
            .into_signal::<TD4>();

        assert_eq!(s.eval(&clk).take(5).collect::<Vec<_>>(), [0, 4, 3, 1, 2]);
    }
}
