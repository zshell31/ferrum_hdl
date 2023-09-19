#![feature(generic_const_exprs)]
#![allow(incomplete_features)]
use ferrum::{
    array::Array,
    domain::ClockDomain,
    signal::{Bundle, Signal, Unbundle},
    unsigned::Unsigned,
};

pub struct TestSystem;

impl ClockDomain for TestSystem {
    const FREQ: usize = 4;
}

pub fn top_module(
    signals: Signal<TestSystem, Array<4, Unsigned<4>>>,
) -> Signal<TestSystem, Array<4, Unsigned<4>>> {
    let [start, .., end] = signals.unbundle().into_inner();

    Array::<4, _>::bundle(
        [
            start.clone(),
            start.clone() + end.clone(),
            start - end.clone(),
            end,
        ]
        .into(),
    )
}

#[cfg(test)]
mod tests {
    use ferrum::{signal::SignalIterExt, CastInner};

    use super::*;

    #[test]
    fn signals() {
        let s = [[0, 1, 2, 3], [1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6]]
            .into_iter()
            .map(CastInner::<Array<4, Unsigned<4>>>::cast_inner)
            .into_signal();

        let res = top_module(s);

        assert_eq!(res.iter().take(4).collect::<Vec<_>>(), [
            [0, 3, 13, 3],
            [1, 5, 13, 4],
            [2, 7, 13, 5],
            [3, 9, 13, 6]
        ]);
    }
}
