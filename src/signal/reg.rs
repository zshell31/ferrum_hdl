use std::borrow::Borrow;

use fhdl_macros::{blackbox, synth};

use super::{Signal, SignalValue};
use crate::{
    domain::{Clock, ClockDomain, Polarity, SyncKind},
    prelude::Unbundle,
};

#[allow(type_alias_bounds)]
pub type Reset<D: ClockDomain> = Signal<D, bool>;

impl<D: ClockDomain> Reset<D> {
    #[synth(inline)]
    pub fn reset() -> Self {
        let rst = Self::lift(false);
        rst
    }
}

#[allow(type_alias_bounds)]
pub type Enable<D: ClockDomain> = Signal<D, bool>;

impl<D: ClockDomain> Enable<D> {
    #[synth(inline)]
    pub fn enable() -> Self {
        let en = Self::lift(true);
        en
    }
}

#[synth(inline)]
pub fn reg<D: ClockDomain, T: SignalValue>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    init: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    let en = Enable::enable();
    reg_en(clk, rst, &en, init, comb_fn)
}

#[synth(inline)]
pub fn reg_comb<D: ClockDomain, T: SignalValue>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    init: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, (T, T)> {
    let en = Enable::enable();
    reg_en_comb(clk, rst, &en, init, comb_fn)
}

#[synth(inline)]
pub fn reg0<D: ClockDomain, T: SignalValue + Default>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    reg(clk, rst, &T::default(), comb_fn)
}

#[synth(inline)]
pub fn reg0_comb<D: ClockDomain, T: SignalValue + Default>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, (T, T)> {
    reg_comb(clk, rst, &T::default(), comb_fn)
}

#[synth(inline)]
pub fn reg_en<D: ClockDomain, T: SignalValue>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    init: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    dff_::<D, T>(
        clk,
        rst,
        en,
        init,
        comb_fn,
        D::RESET_KIND,
        D::RESET_POLARITY,
    )
}

#[synth(inline)]
pub fn reg_en_comb<D: ClockDomain, T: SignalValue>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    init: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, (T, T)> {
    dff_comb_::<D, T>(
        clk,
        rst,
        en,
        init,
        comb_fn,
        D::RESET_KIND,
        D::RESET_POLARITY,
    )
}

#[synth(inline)]
#[inline]
pub fn reg_en0<D: ClockDomain, T: SignalValue + Default>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    reg_en(clk, rst, en, &T::default(), comb_fn)
}

#[synth(inline)]
#[inline]
pub fn reg_en0_comb<D: ClockDomain, T: SignalValue + Default>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, (T, T)> {
    reg_en_comb(clk, rst, en, &T::default(), comb_fn)
}

#[synth(inline)]
pub fn dff<
    D: ClockDomain,
    T: SignalValue,
    const RST_KIND: SyncKind,
    const RST_POL: Polarity,
>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    init: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, T> {
    dff_::<D, T>(clk, rst, en, init, comb_fn, RST_KIND, RST_POL)
}

#[blackbox(SignalDff)]
fn dff_<D: ClockDomain, T: SignalValue>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    init: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
    rst_kind: SyncKind,
    rst_pol: Polarity,
) -> Signal<D, T> {
    let (reg, _) = dff_comb_(clk, rst, en, init, comb_fn, rst_kind, rst_pol).unbundle();
    reg
}

#[synth(inline)]
pub fn dff_comb<
    D: ClockDomain,
    T: SignalValue,
    const RST_KIND: SyncKind,
    const RST_POL: Polarity,
>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    init: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
) -> Signal<D, (T, T)> {
    let reg = dff_comb_(clk, rst, en, init, comb_fn, RST_KIND, RST_POL);
    reg
}

#[blackbox(SignalDffComb)]
fn dff_comb_<D: ClockDomain, T: SignalValue>(
    clk: &Clock<D>,
    rst: &Reset<D>,
    en: &Enable<D>,
    init: &T,
    comb_fn: impl Fn(T) -> T + Clone + 'static,
    rst_kind: SyncKind,
    rst_pol: Polarity,
) -> Signal<D, (T, T)> {
    let clk = clk.clone();
    let mut rst = match rst_pol {
        Polarity::ActiveHigh => rst.clone(),
        Polarity::ActiveLow => !rst,
    };
    let mut en = en.clone();
    let init = init.borrow().clone();

    let mut val = init.clone();
    let mut next_val = init.clone();

    match rst_kind {
        SyncKind::Async => Signal::new(move |ctx| {
            let rst = rst.next(ctx);
            let en = en.next(ctx);
            if rst {
                val = init.clone();
                next_val = (comb_fn)(val.clone());
                (val.clone(), next_val.clone())
            } else if clk.is_rising() && en {
                val = next_val.clone();
                next_val = (comb_fn)(val.clone());
                (val.clone(), next_val.clone())
            } else {
                next_val = (comb_fn)(val.clone());
                (val.clone(), next_val.clone())
            }
        }),
        SyncKind::Sync => Signal::new(move |ctx| {
            let rst = rst.next(ctx);
            let en = en.next(ctx);
            if clk.is_rising() {
                if rst {
                    val = init.clone();
                    next_val = (comb_fn)(val.clone());
                    return (val.clone(), next_val.clone());
                } else if en {
                    val = next_val.clone();
                    next_val = (comb_fn)(val.clone());
                    return (val.clone(), next_val.clone());
                }
            };

            next_val = (comb_fn)(val.clone());
            (val.clone(), next_val.clone())
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cast::{Cast, CastFrom},
        domain::TD4,
        prelude::{Bundle, Eval, U},
    };

    trait TakeByRef: Iterator {
        fn take_by_ref<U: CastFrom<Self::Item>>(&mut self, n: usize) -> Vec<U> {
            self.take(n).map(Cast::cast::<U>).collect::<Vec<_>>()
        }
    }

    impl<I: Iterator> TakeByRef for I {}

    #[test]
    fn test_reg_comb_async_posedge_rst() {
        struct Test;

        impl ClockDomain for Test {
            const FREQ: usize = 4;
            const RESET_KIND: SyncKind = SyncKind::Async;
            const RESET_POLARITY: Polarity = Polarity::ActiveHigh;
        }

        let clk = Clock::<_>::new();
        let rst = Reset::reset();

        let mut r =
            reg_comb::<Test, U<2>>(&clk, &rst, &0_u8.cast(), |val| val + 1).eval(&clk);

        assert_eq!(
            r.take_by_ref::<(u8, u8)>(7),
            //R       F       R       F       R       F       R
            [(0, 1), (0, 1), (1, 2), (1, 2), (2, 3), (2, 3), (3, 0)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //F       R       F       R
            [(0, 1), (0, 1), (0, 1), (0, 1)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(5),
            //F       R       F       R       F
            [(0, 1), (1, 2), (1, 2), (2, 3), (2, 3)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(0, 1), (0, 1), (0, 1), (0, 1)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(1, 2), (1, 2), (2, 3), (2, 3)]
        );
    }

    #[test]
    fn test_reg_sync_posedge_rst() {
        struct Test;

        impl ClockDomain for Test {
            const FREQ: usize = 4;
            const RESET_KIND: SyncKind = SyncKind::Sync;
            const RESET_POLARITY: Polarity = Polarity::ActiveHigh;
        }

        let clk = Clock::<_>::new();
        let rst = Reset::reset();

        let mut r =
            reg_comb::<Test, U<2>>(&clk, &rst, &0_u8.cast(), |val| val + 1).eval(&clk);

        assert_eq!(
            r.take_by_ref::<(u8, u8)>(7),
            //R       F       R       F       R       F       R
            [(0, 1), (0, 1), (1, 2), (1, 2), (2, 3), (2, 3), (3, 0)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //F       R       F       R
            [(3, 0), (0, 1), (0, 1), (0, 1)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(5),
            //F       R       F       R       F
            [(0, 1), (1, 2), (1, 2), (2, 3), (2, 3)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(0, 1), (0, 1), (0, 1), (0, 1)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(1, 2), (1, 2), (2, 3), (2, 3)]
        );
    }

    #[test]
    fn test_reg_async_negedge_rst() {
        struct Test;

        impl ClockDomain for Test {
            const FREQ: usize = 4;
            const RESET_KIND: SyncKind = SyncKind::Async;
            const RESET_POLARITY: Polarity = Polarity::ActiveLow;
        }

        let clk = Clock::<_>::new();
        let rst = Reset::reset();

        let mut r =
            reg_comb::<Test, U<2>>(&clk, &rst, &0_u8.cast(), |val| val + 1).eval(&clk);

        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(0, 1), (0, 1), (0, 1), (0, 1)]
        );

        rst.invert();
        assert_eq!(r.take_by_ref::<(u8, u8)>(9), [
            (1, 2), // R
            (1, 2), // F
            (2, 3), // R
            (2, 3), // F
            (3, 0), // R
            (3, 0), // F
            (0, 1), // R
            (0, 1), // F
            (1, 2)  // R
        ]);

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //F       R       F       R
            [(0, 1), (0, 1), (0, 1), (0, 1)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(5),
            //F       R       F       R       F
            [(0, 1), (1, 2), (1, 2), (2, 3), (2, 3)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(0, 1), (0, 1), (0, 1), (0, 1)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(1, 2), (1, 2), (2, 3), (2, 3)]
        );
    }

    #[test]
    fn test_reg_sync_negedge_rst() {
        struct Test;

        impl ClockDomain for Test {
            const FREQ: usize = 4;
            const RESET_KIND: SyncKind = SyncKind::Sync;
            const RESET_POLARITY: Polarity = Polarity::ActiveLow;
        }

        let clk = Clock::<_>::new();
        let rst = Reset::reset();

        let mut r =
            reg_comb::<Test, U<2>>(&clk, &rst, &0_u8.cast(), |val| val + 1).eval(&clk);

        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(0, 1), (0, 1), (0, 1), (0, 1)]
        );

        rst.invert();
        assert_eq!(r.take_by_ref::<(u8, u8)>(9), [
            (1, 2), // R
            (1, 2), // F
            (2, 3), // R
            (2, 3), // F
            (3, 0), // R
            (3, 0), // F
            (0, 1), // R
            (0, 1), // F
            (1, 2)  // R
        ]);

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //F       R       F       R
            [(1, 2), (0, 1), (0, 1), (0, 1)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(5),
            //F       R       F       R       F
            [(0, 1), (1, 2), (1, 2), (2, 3), (2, 3)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(0, 1), (0, 1), (0, 1), (0, 1)]
        );

        rst.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(1, 2), (1, 2), (2, 3), (2, 3)]
        );
    }

    #[test]
    fn test_reg_en() {
        let clk = Clock::<TD4>::default();
        let rst = Reset::reset();
        let en = Enable::enable();

        let mut r = reg_en_comb::<_, U<2>>(&clk, &rst, &en, &0_u8.cast(), |val| val + 1)
            .eval(&clk);

        assert_eq!(
            r.take_by_ref::<(u8, u8)>(7),
            // R  F  R  F  R  F  R
            [(0, 1), (0, 1), (1, 2), (1, 2), (2, 3), (2, 3), (3, 0)]
        );

        en.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //F       R       F       R
            [(3, 0), (3, 0), (3, 0), (3, 0)]
        );

        en.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(5),
            //F       R       F       R       F
            [(3, 0), (0, 1), (0, 1), (1, 2), (1, 2)]
        );

        en.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(1, 2), (1, 2), (1, 2), (1, 2)]
        );

        en.invert();
        assert_eq!(
            r.take_by_ref::<(u8, u8)>(4),
            //R       F       R       F
            [(2, 3), (2, 3), (3, 0), (3, 0)]
        );
    }

    #[test]
    fn test_reg_seq() {
        let clk = Clock::<TD4>::default();
        let rst_data = Reset::reset();
        let rst = Reset::reset();
        let en = Enable::enable();

        let data = reg(&clk, &rst_data, &1_u8.cast(), |counter: U<3>| counter + 1);

        macro_rules! block {
            ($pred:ident) => {
                $pred.and_then(|data| reg_en0(&clk, &rst, &en, move |_| data.value()))
            };
        }

        let reg0 = block!(data);
        let reg1 = block!(reg0);
        let reg2 = block!(reg1);
        let reg3 = block!(reg2);

        let r = [reg0, reg1, reg2, reg3].bundle();
        let mut r = (data, en.clone(), r).bundle().eval(&clk);

        let cast = |value: (U<_>, bool, [U<_>; 4])| value.cast::<(u8, bool, [u8; 4])>();

        assert_eq!(r.by_ref().take(8).map(cast).collect::<Vec<_>>(), [
            (1, true, [0, 0, 0, 0]), // R
            (1, true, [0, 0, 0, 0]), // F
            (2, true, [1, 0, 0, 0]), // R
            (2, true, [1, 0, 0, 0]), // F
            (3, true, [2, 1, 0, 0]), // R
            (3, true, [2, 1, 0, 0]), // F
            (4, true, [3, 2, 1, 0]), // R
            (4, true, [3, 2, 1, 0])  // F
        ]);

        en.invert();
        assert_eq!(r.by_ref().take(4).map(cast).collect::<Vec<_>>(), [
            (5, false, [3, 2, 1, 0]), // R
            (5, false, [3, 2, 1, 0]), // F
            (6, false, [3, 2, 1, 0]), // R
            (6, false, [3, 2, 1, 0]), // F
        ]);

        en.invert();
        assert_eq!(r.by_ref().take(8).map(cast).collect::<Vec<_>>(), [
            (7, true, [6, 3, 2, 1]), // R
            (7, true, [6, 3, 2, 1]), // F
            (0, true, [7, 6, 3, 2]), // R
            (0, true, [7, 6, 3, 2]), // F
            (1, true, [0, 7, 6, 3]), // R
            (1, true, [0, 7, 6, 3]), // F
            (2, true, [1, 0, 7, 6]), // R
            (2, true, [1, 0, 7, 6]), // F
        ]);

        rst.invert();
        assert_eq!(r.by_ref().take(4).map(cast).collect::<Vec<_>>(), [
            (3, true, [0, 0, 0, 0]), // R
            (3, true, [0, 0, 0, 0]), // F
            (4, true, [0, 0, 0, 0]), // R
            (4, true, [0, 0, 0, 0]), // F
        ]);

        rst.invert();
        assert_eq!(r.by_ref().take(8).map(cast).collect::<Vec<_>>(), [
            (5, true, [4, 0, 0, 0]), // R
            (5, true, [4, 0, 0, 0]), // F
            (6, true, [5, 4, 0, 0]), // R
            (6, true, [5, 4, 0, 0]), // F
            (7, true, [6, 5, 4, 0]), // R
            (7, true, [6, 5, 4, 0]), // F
            (0, true, [7, 6, 5, 4]), // R
            (0, true, [7, 6, 5, 4]), // F
        ]);
    }
}
