#![allow(incomplete_features)]
#![feature(generic_const_exprs)]

use ferrum::{
    bit::Bit,
    bit_pack::BitPack,
    const_asserts::{Assert, IsTrue},
    const_functions::clog2,
    domain::ClockDomain,
    signal::{reg, Clock, Signal},
    unsigned::{is_unsigned, Unsigned},
};

pub const fn second_periods<D: ClockDomain>() -> usize {
    D::PICOSECONDS / D::PERIOD
}

pub const fn blinking_count<D: ClockDomain>() -> u8 {
    clog2(second_periods::<D>()) as u8
}

pub fn blinking<D: ClockDomain>(clk: Clock<D>) -> impl Signal<D, Value = Bit>
where
    Assert<{ is_unsigned(blinking_count::<D>()) }>: IsTrue,
{
    reg::<D, _>(clk, 0, |r: Unsigned<{ blinking_count::<D>() }>| r + 1)
        .smap(|value| value.msb())
}

pub struct ZynqMiniDom;

impl ClockDomain for ZynqMiniDom {
    const FREQ: usize = 50_000_000;
}

#[cfg(not(test))]
pub fn top_module(clk: Clock<ZynqMiniDom>) -> impl Signal<ZynqMiniDom, Value = Bit> {
    blinking(clk)
}

#[cfg(test)]
mod tests {
    use super::*;

    pub struct TestSystem;

    impl ClockDomain for TestSystem {
        const FREQ: usize = 4;
    }

    fn top_module(clk: Clock<TestSystem>) -> impl Signal<TestSystem, Value = Bit> {
        blinking(clk)
    }

    #[test]
    fn signals() {
        assert_eq!(
            top_module(Default::default())
                .iter()
                .take(8)
                .map(bool::from)
                .collect::<Vec<_>>(),
            &[false, false, true, true, false, false, true, true]
        );
    }
}
