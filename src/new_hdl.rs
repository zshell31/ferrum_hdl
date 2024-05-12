use std::{marker::PhantomData, usize};

pub use fhdl_macros::State;
use fhdl_macros::{blackbox, blackbox_ty, lang_item, synth};

use crate::{
    domain::{hz_to_period, Polarity, SyncKind},
    prelude::{ConstConstr, Idx, SignalValue},
    rise_every_constr,
};

pub trait State: Sized {
    type Mut<'a>: StateMut
    where
        Self: 'a;

    fn state() -> Self;

    fn as_mut(&mut self) -> Self::Mut<'_>;
}

pub trait StateMut: Sized {}

#[lang_item(Module)]
pub trait Module: StateMut {
    type Input: SignalValue;
    type Output: SignalValue;

    #[lang_item(ModLogic)]
    fn logic(self, inputs: Self::Input) -> Self::Output;
}

pub const fn clk_divider<D: ClockDomain>(ps: usize) -> usize {
    assert!(ps >= D::PERIOD);
    ps / D::PERIOD
}

#[lang_item(Domain)]
pub trait ClockDomain: 'static {
    #[lang_item(DomFreq)]
    const FREQ: usize;
    const PERIOD: usize = hz_to_period(Self::FREQ);

    #[lang_item(DomRstKind)]
    const RST_KIND: SyncKind;
    #[lang_item(DomRstPol)]
    const RST_POLARITY: Polarity;

    fn set_clk(clk: bool);

    fn revert() {
        Self::set_clk(!Self::clk());
    }

    fn set_rst(rst: bool);

    fn clk() -> bool;

    fn rst_() -> bool;

    fn rst() -> bool {
        Self::RST_POLARITY.bool(Self::rst_())
    }
}

pub struct Reg<D: ClockDomain, T: SignalValue> {
    state: Option<Reg_<T>>,
    _domain: PhantomData<D>,
}

struct Reg_<T: SignalValue> {
    val: T,
    init_val: T,
}

#[blackbox_ty(Reg)]
pub struct RegMut<'a, D: ClockDomain, T: SignalValue>(&'a mut Reg<D, T>);

impl<'a, D: ClockDomain, T: SignalValue> StateMut for RegMut<'a, D, T> {}

impl<D: ClockDomain, T: SignalValue> State for Reg<D, T> {
    type Mut<'a> = RegMut<'a, D, T>;

    fn state() -> Self {
        Self {
            state: None,
            _domain: PhantomData,
        }
    }

    fn as_mut(&mut self) -> Self::Mut<'_> {
        RegMut(self)
    }
}

impl<'a, D: ClockDomain, T: SignalValue> RegMut<'a, D, T> {
    #[blackbox(RegEn)]
    pub fn reg_en(self, init_val: T, en: bool, f: impl Fn(T) -> T) -> T {
        if self.0.state.is_none() {
            self.0.state = Some(Reg_ {
                val: init_val.clone(),
                init_val,
            });
        }

        let state = self.0.state.as_mut().unwrap();
        match D::RST_KIND {
            SyncKind::Sync => {
                if D::clk() {
                    if D::rst() {
                        state.val = state.init_val.clone();
                    } else if en {
                        state.val = f(state.val.clone());
                    }
                }
            }
            SyncKind::Async => {
                if D::rst() {
                    state.val = state.init_val.clone();
                } else if D::clk() && en {
                    state.val = f(state.val.clone());
                }
            }
        }

        state.val.clone()
    }

    #[blackbox(RegEnComb)]
    pub fn reg_en_comb(
        self,
        init_val: T,
        en: bool,
        f: impl Fn(T) -> T + Clone,
    ) -> (T, T) {
        let val = self.reg_en(init_val, en, f.clone());
        (val.clone(), f(val))
    }

    #[synth(inline)]
    pub fn reg_en0(self, en: bool, f: impl Fn(T) -> T) -> T
    where
        T: Default,
    {
        self.reg_en(T::default(), en, f)
    }

    #[synth(inline)]
    pub fn reg(self, init_val: T, f: impl Fn(T) -> T) -> T {
        self.reg_en(init_val, true, f)
    }

    #[synth(inline)]
    pub fn reg0(self, f: impl Fn(T) -> T) -> T
    where
        T: Default,
    {
        self.reg_en0(true, f)
    }
}

#[derive(State)]
pub struct RiseEvery<D: ClockDomain, const PS: usize>
where
    ConstConstr<{ rise_every_constr!(PS) }>:,
{
    pub cnt: Reg<D, Idx<PS>>,
}

impl<D: ClockDomain, const PS: usize> Module for RiseEveryMut<'_, D, PS>
where
    ConstConstr<{ rise_every_constr!(PS) }>:,
{
    type Input = ();
    type Output = bool;

    #[synth(inline)]
    fn logic(self, _: Self::Input) -> Self::Output {
        let cnt = self.cnt.reg0(|idx| idx.succ());
        let en = cnt.is_max();
        en
    }
}

#[macro_export]
macro_rules! rise_period_constr1 {
    ($domain:ident, $period:expr) => {
        $crate::rise_every_constr!($crate::new_hdl::clk_divider::<$domain>($period))
    };
}

#[derive(State)]
pub struct RisePeriod<D: ClockDomain, const PS: usize>
where
    ConstConstr<{ rise_period_constr1!(D, PS) }>:,
{
    pub cnt: RiseEvery<D, { clk_divider::<D>(PS) }>,
}

impl<D: ClockDomain, const PS: usize> Module for RisePeriodMut<'_, D, PS>
where
    ConstConstr<{ rise_period_constr1!(D, PS) }>:,
{
    type Input = ();
    type Output = bool;

    #[synth(inline)]
    fn logic(self, inputs: Self::Input) -> Self::Output {
        self.cnt.logic(inputs)
    }
}
