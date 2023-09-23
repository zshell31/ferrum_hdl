#![allow(incomplete_features)]
#![feature(generic_const_exprs)]

use ferrum::{
    bit::Bit,
    bit_pack::BitPack,
    const_functions::clog2,
    const_helpers::UsizeConstr,
    domain::{Clock, ClockDomain, PICOSECONDS},
    signal::{reg, Reset, Signal},
    unsigned::Unsigned,
};

pub const fn second_periods<D: ClockDomain>() -> usize {
    PICOSECONDS / D::PERIOD
}

pub const fn blinking_count<D: ClockDomain>() -> usize {
    clog2(second_periods::<D>())
}

pub const fn hz_to_period(freq: usize) -> usize {
    PICOSECONDS / freq
}

pub const fn clock_divider<D: ClockDomain>(ps: usize) -> usize {
    ps / D::PERIOD
}

pub fn blinking<D: ClockDomain>(
    clk: Clock<D>,
    rst: Reset<D>,
) -> Signal<D, (Bit, Unsigned<{ blinking_count::<D>() }>)>
where
    UsizeConstr<{ blinking_count::<D>() }>:,
{
    reg::<D, _>(
        clk,
        rst,
        0.into(),
        |r: Unsigned<{ blinking_count::<D>() }>, _| r + 1,
    )
    .map(|value| (value.msb(), value))
}

pub struct ZynqMiniDom;

impl ClockDomain for ZynqMiniDom {
    const FREQ: usize = 50_000_000;
}

#[cfg(not(test))]
const BL_COUNT: usize = blinking_count::<ZynqMiniDom>();

#[cfg(not(test))]
pub fn top_module(
    clk: Clock<ZynqMiniDom>,
    rst: Reset<ZynqMiniDom>,
) -> Signal<ZynqMiniDom, (Bit, Unsigned<BL_COUNT>)> {
    blinking(clk, rst)
}

#[cfg(test)]
mod tests {
    use super::*;

    pub struct TestSystem;

    impl ClockDomain for TestSystem {
        const FREQ: usize = 4;
    }

    const BL_COUNT: u8 = blinking_count::<TestSystem>();

    fn top_module(
        clk: Clock<TestSystem>,
        rst: Reset<TestSystem>,
    ) -> Signal<TestSystem, (Bit, Unsigned<BL_COUNT>)> {
        blinking(clk, rst)
    }

    #[test]
    fn signals() {
        assert_eq!(
            top_module(Default::default(), Reset::reset())
                .iter()
                .take(8)
                .map(|(led, count)| (bool::from(led), u128::from(count)))
                .collect::<Vec<_>>(),
            &[
                (false, 0b00),
                (false, 0b01),
                (true, 0b10),
                (true, 0b11),
                (false, 0b00),
                (false, 0b01),
                (true, 0b10),
                (true, 0b11)
            ]
        );
    }
}
