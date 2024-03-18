#![allow(incomplete_features)]
#![feature(generic_const_exprs)]

use ferrum_hdl::prelude::*;

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
    ConstConstr<{ idx_constr(blinking_count::<D>()) }>:,
{
    reg::<D, _>(
        &clk,
        &rst,
        &0_u8.cast(),
        |r: Unsigned<{ blinking_count::<D>() }>| r + 1,
    )
    .map(|value| (value.msb(), value))
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
