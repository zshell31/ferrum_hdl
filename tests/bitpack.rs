#![allow(incomplete_features)]
#![feature(generic_const_exprs)]

use ferrum_hdl::{
    array::Array,
    bit::Bit,
    bitpack::{BitPack, BitSize},
    signal::SignalValue,
    unsigned::Unsigned,
};

mod test_zs_struct {

    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    struct Test {}

    #[test]
    fn bitsize() {
        assert_eq!(Test::BITS, 0);
    }

    #[test]
    fn pack() {
        let s = Test {};

        assert_eq!(s.pack(), 0_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test = BitPack::unpack(0_u64.into());

        assert_eq!(s, Test {});
    }
}

mod test_struct {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    struct Test {
        a: Unsigned<4>,
        b: Bit,
        c: Array<2, Unsigned<2>>,
    }

    #[test]
    fn bitsize() {
        assert_eq!(Test::BITS, 9);
    }

    #[test]
    fn pack() {
        let s = Test {
            a: 12_u8.into(),
            b: false.into(),
            c: [1_u8.into(), 3_u8.into()].into(),
        };

        assert_eq!(s.pack(), 0b110000111_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test = BitPack::unpack(0b110000111_u64.into());

        assert_eq!(s, Test {
            a: 12_u8.into(),
            b: false.into(),
            c: [1_u8.into(), 3_u8.into()].into(),
        });
    }
}

mod test_struct_with_type_param {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    struct Test<T> {
        a: T,
        b: Bit,
    }

    #[test]
    fn bitsize() {
        assert_eq!(Test::<Unsigned<3>>::BITS, 4);
    }

    #[test]
    fn pack() {
        let s = Test::<Unsigned<4>> {
            a: 12u8.into(),
            b: false.into(),
        };

        assert_eq!(s.pack(), 0b11000_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test<Unsigned<4>> = BitPack::unpack(0b11000_u64.into());

        assert_eq!(s, Test {
            a: 12_u8.into(),
            b: false.into(),
        });
    }
}

mod test_struct_with_const_param {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    struct Test<const N: usize> {
        a: Unsigned<N>,
        b: Bit,
    }

    #[test]
    fn bitsize() {
        assert_eq!(Test::<2>::BITS, 3);
    }

    #[test]
    fn pack() {
        let s = Test::<4> {
            a: 12u8.into(),
            b: false.into(),
        };

        assert_eq!(s.pack(), 0b11000_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test<4> = BitPack::unpack(0b11000_u64.into());

        assert_eq!(s, Test {
            a: 12_u8.into(),
            b: false.into(),
        });
    }
}

mod test_struct_with_type_const_param {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    struct Test<const N: usize, T> {
        a: Unsigned<N>,
        b: T,
    }

    #[test]
    fn bitsize() {
        assert_eq!(Test::<4, Unsigned<2>>::BITS, 6);
    }

    #[test]
    fn pack() {
        let s = Test::<4, Bit> {
            a: 12u8.into(),
            b: false.into(),
        };

        assert_eq!(s.pack(), 0b11000_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test<4, Bit> = BitPack::unpack(0b11000_u64.into());

        assert_eq!(s, Test {
            a: 12_u8.into(),
            b: false.into(),
        });
    }
}

mod test_zs_enum {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    enum Test {}

    #[test]
    fn bitsize() {
        assert_eq!(Test::BITS, 0)
    }
}

mod test_enum_with_1_variant {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    enum Test {
        A(),
    }

    #[test]
    fn bitsize() {
        assert_eq!(Test::BITS, 1);
    }

    #[test]
    fn pack() {
        let s = Test::A();

        assert_eq!(s.pack(), 0b0_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test = BitPack::unpack(0b0_u64.into());

        assert_eq!(s, Test::A());
    }
}

mod test_enum_with_1_var_and_type_param {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    enum Test<T> {
        A(T),
    }

    #[test]
    fn bitsize() {
        assert_eq!(Test::<Unsigned<3>>::BITS, 4);
    }

    #[test]
    fn pack() {
        let s = Test::<Unsigned<3>>::A(5_u8.into());

        assert_eq!(s.pack(), 0b0101_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test<Unsigned<3>> = BitPack::unpack(0b0101_u64.into());

        assert_eq!(s, Test::A(5_u8.into()));
    }
}

mod test_enum_with_1_var_and_const_param {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    enum Test<const N: usize> {
        A(Unsigned<N>),
    }

    #[test]
    fn bitsize() {
        assert_eq!(Test::<3>::BITS, 4);
    }

    #[test]
    fn pack() {
        let s = Test::<3>::A(5_u8.into());

        assert_eq!(s.pack(), 0b0101_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test<3> = BitPack::unpack(0b0101_u64.into());

        assert_eq!(s, Test::A(5_u8.into()));
    }
}

mod test_enum_with_2_variants {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    enum Test<const N: usize, T> {
        A(Unsigned<N>),
        B(Bit, T),
    }

    #[test]
    fn bitsize() {
        assert_eq!(Test::<3, Array<3, Bit>>::BITS, 5);
    }

    #[test]
    fn pack() {
        let s = Test::<3, Array<3, Bit>>::B(
            true.into(),
            [false.into(), true.into(), true.into()].into(),
        );

        assert_eq!(s.pack(), 0b11011_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test<3, Array<3, Bit>> = BitPack::unpack(0b11011_u64.into());

        assert_eq!(
            s,
            Test::<3, Array<3, Bit>>::B(
                true.into(),
                [false.into(), true.into(), true.into()].into(),
            )
        );
    }
}

mod test_enum_with_3_variants {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    #[bitpack(bound = "[(); <Array<N, Bit> as BitSize>::BITS]:")]
    enum Test<const N: usize, T> {
        A(Unsigned<1>),
        B(Bit),
        C { a: Bit, b: T, c: Array<N, Bit> },
    }

    #[test]
    fn bitsize() {
        assert_eq!(Test::<3, Array<3, Bit>>::BITS, 9);
    }

    #[test]
    fn pack() {
        let s = Test::<3, Unsigned<3>>::C {
            a: false.into(),
            b: 5_u8.into(),
            c: [true.into(), false.into(), true.into()].into(),
        };

        assert_eq!(s.pack(), 0b100101101_u64.into());

        let s = Test::<3, Unsigned<3>>::B(true.into());

        assert_eq!(s.pack(), 0b011000000_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test<3, Unsigned<3>> = BitPack::unpack(0b100101101_u64.into());

        assert_eq!(s, Test::<3, Unsigned<3>>::C {
            a: false.into(),
            b: 5_u8.into(),
            c: [true.into(), false.into(), true.into()].into()
        });

        let s: Test<3, Unsigned<3>> = BitPack::unpack(0b011000000_u64.into());

        assert_eq!(s, Test::<3, Unsigned<3>>::B(true.into()));
    }
}

mod test_nested_adts {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    #[bitpack(bound = "[(); <Array<N, Bit> as BitSize>::BITS]:")]
    struct Test1<const N: usize> {
        a: Array<N, Bit>,
    }

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    #[bitpack(bound = "[(); <Array<N, Bit> as BitSize>::BITS]:")]
    #[bitpack(bound = "[(); <Test1<N> as BitSize>::BITS]:")]
    struct Test2<const N: usize> {
        b: Test1<N>,
    }

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    enum Test3<T> {
        A(T),
    }

    type Test4 = Test3<Test2<3>>;

    #[test]
    fn bitsize() {
        assert_eq!(Test4::BITS, 4);
    }

    #[test]
    fn pack() {
        let s = Test4::A(Test2 {
            b: Test1 {
                a: [true.into(), false.into(), false.into()].into(),
            },
        });

        assert_eq!(s.pack(), 0b0100_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test4 = BitPack::unpack(0b0100_u64.into());

        assert_eq!(
            s,
            Test4::A(Test2 {
                b: Test1 {
                    a: [true.into(), false.into(), false.into()].into(),
                },
            })
        );
    }
}

mod test_struct_with_phantom {
    use std::marker::PhantomData;

    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    #[bitpack(bound = "T:")]
    struct Test<T> {
        _marker: PhantomData<T>,
        a: Bit,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct Foo {}

    #[test]
    fn bitsize() {
        assert_eq!(Test::<Foo>::BITS, 1);
    }

    #[test]
    fn pack() {
        let s = Test::<Foo> {
            _marker: PhantomData,
            a: true.into(),
        };

        assert_eq!(s.pack(), 0b1_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test<Foo> = BitPack::unpack(0b1_u64.into());

        assert_eq!(s, Test::<Foo> {
            _marker: PhantomData,
            a: true.into()
        });
    }
}

mod test_enum_with_phantom {
    use std::marker::PhantomData;

    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, SignalValue, BitPack)]
    #[bitpack(bound = "T:")]
    enum Test<T> {
        A(Bit),
        B(PhantomData<T>),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct Foo {}

    #[test]
    fn bitsize() {
        assert_eq!(Test::<Foo>::BITS, 2);
    }

    #[test]
    fn pack() {
        let s = Test::<Foo>::B(PhantomData);

        assert_eq!(s.pack(), 0b10_u64.into());
    }

    #[test]
    fn unpack() {
        let s: Test<Foo> = BitPack::unpack(0b10_u64.into());

        assert_eq!(s, Test::<Foo>::B(PhantomData));
    }
}
