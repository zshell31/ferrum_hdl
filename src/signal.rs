mod ops;
mod reg;
mod sim;
mod wrapped;

use std::{borrow::Cow, cell::RefCell, marker::PhantomData, rc::Rc};

use derive_where::derive_where;
pub use fhdl_macros::SignalValue;
use fhdl_macros::{blackbox, blackbox_ty, synth};
pub use ops::IntoSignal;
pub use reg::{dff, reg, reg0, reg_en, reg_en0, Enable, Reset};
pub use sim::{SignalIterExt, Source};
pub use wrapped::Wrapped;

use crate::{
    bit::Bit,
    domain::{Clock, ClockDomain},
    signal_fn::SignalFn,
    simulation::SimCtx,
    watchable::{AsDisplay, FmtKind, Formatter, Watchable},
};

pub trait SignalValue: Clone + 'static {}

impl<T: SignalValue> SignalValue for Option<T> {}

#[derive_where(Debug, Clone; T)]
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

    #[blackbox(IntoSignal)]
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

    #[synth(inline)]
    pub fn reg<U: SignalValue>(
        &self,
        clk: Clock<D>,
        rst: &Reset<D>,
        f: impl Fn(T) -> U + Clone + 'static,
    ) -> Signal<D, U>
    where
        U: Default,
    {
        self.and_then(|value| reg0(clk, rst, move |_| f(value.value())))
    }

    #[synth(inline)]
    pub fn reg_en<U: SignalValue>(
        &self,
        clk: Clock<D>,
        rst: &Reset<D>,
        en: &Enable<D>,
        f: impl Fn(T) -> U + Clone + 'static,
    ) -> Signal<D, U>
    where
        U: Default,
    {
        self.and_then(|value| reg_en0(clk, rst, en, move |_| f(value.value())))
    }

    #[synth(inline)]
    pub fn into_reg(&self, clk: Clock<D>, rst: &Reset<D>) -> Signal<D, T>
    where
        T: Default,
    {
        self.and_then(|value| reg0(clk, rst, move |_| value.value()))
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

#[cfg(test)]
mod tests {
    use std::iter;

    use super::{SignalIterExt, *};
    use crate::{
        bundle::Bundle,
        cast::{Cast, CastFrom},
        domain::TD4,
        prelude::Simulate,
        unsigned::Unsigned,
    };

    #[test]
    fn test_iter() {
        let s = [0_u8, 4, 3, 1, 2]
            .into_iter()
            .map(Unsigned::<8>::cast_from)
            .into_signal::<TD4>();

        assert_eq!(s.simulate().take(5).collect::<Vec<_>>(), [0, 4, 3, 1, 2]);
    }

    #[test]
    fn test_reg() {
        let clk = Clock::<TD4>::default();
        let (rst, rst_signal) = Reset::reset_src();

        let mut r =
            reg::<TD4, Unsigned<3>>(clk, &rst_signal, &0_u8.cast(), |val| val + 1)
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
        let clk = Clock::<TD4>::default();
        let (rst, rst_signal) = Reset::reset_src();
        let (en, en_signal) = Enable::enable_src();

        let mut r =
            reg_en::<_, Unsigned<3>>(clk, &rst_signal, &en_signal, &0_u8.cast(), |val| {
                val + 1
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
        let clk = Clock::<TD4>::default();
        let (rst, rst_signal) = Reset::reset_src();
        let (en, en_signal) = Enable::enable_src();

        let data = iter::successors::<Unsigned<3>, _>(Some(1_u8.cast()), |val| {
            Some(val.clone() + 1)
        })
        .into_signal::<TD4>();

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
