use fhdl_macros::synth;

use super::{
    reg::{reg0, Reset},
    Signal,
};
use crate::{
    bit::Bit,
    const_helpers::ConstConstr,
    domain::{clk_divider, hz_to_period, Clock, ClockDomain},
    index::Idx,
};

#[macro_export]
macro_rules! rise_every_constr {
    ($n:expr) => {
        $crate::index::idx_constr($n)
    };
}

#[synth(inline)]
pub fn rise_every<D: ClockDomain, const PS: usize>(
    clk: &Clock<D>,
    rst: &Reset<D>,
) -> Signal<D, bool>
where
    ConstConstr<{ rise_every_constr!(PS) }>:,
{
    reg0(clk, rst, |(idx, _): (Idx<PS>, Bit)| {
        (idx.clone().succ(), idx.is_max())
    })
    .map(|(_, en)| en)
}

#[macro_export]
macro_rules! rise_period_constr {
    ($domain:ident, $period:expr) => {
        $crate::rise_every_constr!($crate::domain::clk_divider::<$domain>($period))
    };
}

#[synth(inline)]
pub fn rise_period<D: ClockDomain, const PS: usize>(
    clk: &Clock<D>,
    rst: &Reset<D>,
) -> Signal<D, bool>
where
    ConstConstr<{ rise_period_constr!(D, PS) }>:,
{
    rise_every::<D, { clk_divider::<D>(PS) }>(clk, rst)
}

#[macro_export]
macro_rules! rise_rate_constr {
    ($domain:ident, $rate:expr) => {
        $crate::rise_period_constr!($domain, $crate::domain::hz_to_period($rate))
    };
}

#[synth(inline)]
pub fn rise_rate<D: ClockDomain, const RATE: usize>(
    clk: &Clock<D>,
    rst: &Reset<D>,
) -> Signal<D, bool>
where
    ConstConstr<{ rise_rate_constr!(D, RATE) }>:,
{
    rise_period::<D, { hz_to_period(RATE) }>(clk, rst)
}
