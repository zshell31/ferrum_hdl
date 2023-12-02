use fhdl_macros::impl_tuple_traits;

use crate::{
    bitpack::{BitPack, BitSize, IsPacked},
    bitvec::BitVec,
    cast::{Cast, CastFrom},
    domain::ClockDomain,
    signal::{Bundle, Signal, SignalValue, Unbundle},
    simulation::{SimCtx, Simulate},
};

impl_tuple_traits!(1);
impl_tuple_traits!(2);
impl_tuple_traits!(3);
impl_tuple_traits!(4);
impl_tuple_traits!(5);
impl_tuple_traits!(6);
impl_tuple_traits!(7);
impl_tuple_traits!(8);
impl_tuple_traits!(9);
impl_tuple_traits!(10);
impl_tuple_traits!(11);
impl_tuple_traits!(12);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        array::Array, bit::Bit, domain::TestSystem4, signal::SignalIterExt,
        unsigned::Unsigned,
    };

    #[test]
    fn unbundle() {
        let s: Signal<TestSystem4, (Unsigned<4>, Bit)> = [
            (0_u8, false),
            (1, true),
            (2, true),
            (3, false),
            (4, true),
            (5, false),
        ]
        .into_iter()
        .map(Cast::cast)
        .into_signal();

        let res = s.unbundle();

        assert_eq!(
            res.simulate()
                .take(6)
                .map(Cast::cast::<(u8, bool)>)
                .collect::<Vec<_>>(),
            [
                (0, false),
                (1, true),
                (2, true),
                (3, false),
                (4, true),
                (5, false),
            ]
        );
    }

    #[test]
    fn bundle() {
        let s: (Signal<TestSystem4, Unsigned<4>>, Signal<TestSystem4, Bit>) = (
            [0_u8, 1, 2, 3, 4, 5]
                .into_iter()
                .map(Cast::cast)
                .into_signal(),
            [false, true, true, false, true, false]
                .into_iter()
                .map(Cast::cast)
                .into_signal(),
        );

        let res = s.bundle();

        assert_eq!(
            res.simulate()
                .take(6)
                .map(Cast::cast::<(u8, bool)>)
                .collect::<Vec<_>>(),
            [
                (0, false),
                (1, true),
                (2, true),
                (3, false),
                (4, true),
                (5, false),
            ]
        );
    }

    #[test]
    fn pack() {
        let s: (Unsigned<4>, Bit, Array<2, Unsigned<2>>) =
            (12_u8.into(), false.into(), [1_u8.into(), 3_u8.into()]);

        assert_eq!(s.pack(), 0b110000111_u64.into());
    }

    #[test]
    fn unpack() {
        let s: (Unsigned<4>, Bit, Array<2, Unsigned<2>>) =
            BitPack::unpack(0b110000111_u64.into());

        assert_eq!(s, (12_u8.into(), false.into(), [1_u8.into(), 3_u8.into()]));
    }
}
