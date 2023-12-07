#![allow(incomplete_features)]
#![feature(generic_const_exprs)]

use ferrum_hdl::{
    bit::Bit,
    bitpack::BitPackExt,
    cast::Cast,
    const_functions::clog2,
    const_helpers::ConstConstr,
    domain::{clk_divider, Clock, ClockDomain, SECOND},
    signal::{reg, Reset, Signal},
    unsigned::Unsigned,
};

pub const fn second_periods<D: ClockDomain>() -> usize {
    clk_divider::<D>(SECOND)
}

pub const fn blinking_count<D: ClockDomain>() -> usize {
    clog2(second_periods::<D>())
}

pub fn blinking<D: ClockDomain>(
    clk: Clock<D>,
    rst: Reset<D>,
) -> Signal<D, (Bit, Unsigned<{ blinking_count::<D>() }>)>
where
    ConstConstr<{ blinking_count::<D>() }>:,
{
    reg::<D, _>(
        clk,
        rst,
        &0_u8.cast(),
        |r: Unsigned<{ blinking_count::<D>() }>| r + 1_u8,
    )
    .map(|value| (value.clone().msb().cast(), value))
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
    use ferrum_hdl::domain::TestSystem4;

    use super::*;

    const BL_COUNT: u8 = blinking_count::<TestSystem4>();

    #[test]
    fn signals() {
        assert_eq!(
            blinking::<TestSystem4>(Default::default(), Reset::reset())
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
