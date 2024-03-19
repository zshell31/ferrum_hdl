use std::{
    cmp::Ordering::{self, *},
    fmt::{self, Binary, Display, LowerHex},
    io,
    marker::StructuralPartialEq,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub},
};

use fhdl_const_func::mask;
use fhdl_macros::{blackbox, blackbox_ty, synth};
use num_bigint::BigUint;
use num_traits::Zero;
use paste::paste;
use vcd::IdCode;

use crate::{
    bit::Bit,
    bitpack::{BitPack, BitSize, BitVec},
    cast::{Cast, CastFrom},
    const_functions::slice_len,
    const_helpers::{Assert, ConstConstr, IsTrue},
    index::{idx_constr, Idx},
    signal::SignalValue,
    trace::{bool_to_vcd, TraceTy, TraceVars, Traceable, Tracer},
};

#[derive(Debug, Clone)]
#[blackbox_ty(Unsigned)]
pub enum U<const N: usize> {
    Short(u128),
    Long(BigUint),
}

impl<const N: usize> U<N>
where
    Assert<{ N <= 128 }>: IsTrue,
{
    pub const fn from(val: u128) -> Self {
        Self::Short(val)
    }
}

impl<const N: usize> U<N> {
    fn from_short(val: u128) -> Self {
        match N.cmp(&128) {
            Ordering::Less => {
                let mask = (1 << N) - 1;
                Self::Short(val & mask)
            }
            Ordering::Equal => Self::Short(val),
            Ordering::Greater => {
                let mask = (BigUint::from(1_u8) << N) - 1_u8;
                Self::Long(BigUint::from(val) & mask)
            }
        }
    }

    fn from_long(val: BigUint) -> Self {
        match N.cmp(&128) {
            Ordering::Less => {
                let mask = (1 << N) - 1;
                Self::Short(u128::try_from(val).unwrap() & mask)
            }
            Ordering::Equal => Self::Short(u128::try_from(val).unwrap()),
            Ordering::Greater if val.bits() as usize == N => Self::Long(val),
            _ => {
                let mask = (BigUint::from(1_u8) << N) - 1_u8;
                Self::Long(val & mask)
            }
        }
    }

    fn bit_(&self, n: usize) -> Bit {
        if n >= N {
            return false;
        }

        match self {
            Self::Short(short) => (short & (1 << n)) > 0,
            Self::Long(long) => long.bit(n as u64),
        }
    }

    pub(crate) fn is_non_zero(&self) -> Bit {
        match self {
            Self::Short(short) => *short != 0,
            Self::Long(long) => !long.is_zero(),
        }
    }

    #[blackbox(Index)]
    pub fn idx(&self, idx: Idx<N>) -> Bit
    where
        ConstConstr<{ idx_constr(N) }>:,
    {
        self.bit_(idx.cast())
    }

    #[synth(inline)]
    pub fn msb(self) -> Bit
    where
        ConstConstr<{ idx_constr(N) }>:,
    {
        self.idx(unsafe { Idx::from_usize(N - 1) })
    }

    #[blackbox(Slice)]
    pub fn slice<const M: usize>(&self, idx: Idx<{ slice_len(N, M) }>) -> U<M>
    where
        ConstConstr<{ idx_constr(slice_len(N, M)) }>:,
    {
        let idx: usize = idx.cast();
        match self {
            Self::Short(short) => {
                let mask = mask(M as u128);
                U::<M>::from_short((short >> idx) & mask)
            }
            Self::Long(long) => {
                let mask = (BigUint::from(1_u8) << M) - 1_u8;
                U::<M>::from_long((long >> idx) & mask)
            }
        }
    }

    #[synth(inline)]
    pub fn rotate_left(self) -> Self {
        (self.clone() << 1_u128) | ((self >> (N - 1)) & 1)
    }
}

impl<const N: usize> SignalValue for U<N> {}

impl<const N: usize> BitSize for U<N> {
    const BITS: usize = N;
}

impl<const N: usize> BitPack for U<N> {
    type Packed = BitVec<N>;

    fn pack(self) -> Self::Packed {
        self.clone()
    }

    fn unpack(bitvec: Self::Packed) -> Self {
        bitvec.clone()
    }
}

impl<const N: usize> Default for U<N> {
    #[synth(inline)]
    #[inline]
    fn default() -> Self {
        0_u8.cast()
    }
}

impl<const N: usize> Display for U<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Short(short) => Display::fmt(short, f),
            Self::Long(long) => Display::fmt(long, f),
        }
    }
}

impl<const N: usize> Binary for U<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Short(short) => write!(f, "{:0width$b}", short, width = N),
            Self::Long(long) => write!(f, "{:0width$b}", long, width = N),
        }
    }
}

impl<const N: usize> LowerHex for U<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let width = N.div_ceil(8);
        match self {
            Self::Short(short) => write!(f, "{:0width$x}", short, width = width),
            Self::Long(long) => write!(f, "{:0width$x}", long, width = width),
        }
    }
}

impl<const N: usize> PartialEq for U<N> {
    #[blackbox(OpEq)]
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Short(lhs), Self::Short(rhs)) => lhs == rhs,
            (Self::Long(lhs), Self::Long(rhs)) => lhs == rhs,
            _ => unreachable!(),
        }
    }

    #[allow(clippy::partialeq_ne_impl)]
    #[blackbox(OpNe)]
    #[inline]
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl<const N: usize> StructuralPartialEq for U<N> {}

impl<const N: usize> Eq for U<N> {}

impl<const N: usize> PartialOrd for U<N> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }

    #[blackbox(OpLt)]
    #[inline]
    fn lt(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(Less))
    }

    #[blackbox(OpLe)]
    #[inline]
    fn le(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(Less | Equal))
    }

    #[blackbox(OpGt)]
    #[inline]
    fn gt(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(Greater))
    }

    #[blackbox(OpGe)]
    #[inline]
    fn ge(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(Greater | Equal))
    }
}

impl<const N: usize> Ord for U<N> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Short(lhs), Self::Short(rhs)) => lhs.cmp(rhs),
            (Self::Long(lhs), Self::Long(rhs)) => lhs.cmp(rhs),
            _ => unreachable!(),
        }
    }
}

macro_rules! impl_for_unsigned_prim_ty {
    ($( $prim:ty ),+) => {
        $(
            impl SignalValue for $prim {}

            impl <const N: usize> CastFrom<$prim> for U<N> {
                #[blackbox(CastFrom)]
                #[inline]
                fn cast_from(val: $prim) -> Self {
                    Self::from_short(val as u128)
                }
            }

            impl <const N: usize> CastFrom<U<N>> for $prim {
                #[blackbox(CastFrom)]
                #[inline]
                fn cast_from(val: U<N>) -> Self {
                    match val {
                        U::Short(short) => short as $prim,
                        U::Long(long) => long.try_into().unwrap()
                    }
                }
            }

            impl BitSize for $prim {
                const BITS: usize = <$prim>::BITS as usize;
            }

            impl BitPack for $prim {
                type Packed = BitVec<{ <$prim as BitSize>::BITS }>;

                #[inline]
                fn pack(self) -> Self::Packed {
                    self.cast()
                }

                #[inline]
                fn unpack(bitvec: Self::Packed) -> Self {
                    bitvec.cast()
                }
            }

        )+
    };
}

impl_for_unsigned_prim_ty!(u8, u16, u32, u64, u128, usize);

impl<const N: usize> PartialEq<u128> for U<N> {
    #[synth(inline)]
    #[inline]
    fn eq(&self, other: &u128) -> bool {
        self.eq(&Self::cast_from(*other))
    }
}

impl<const N: usize> PartialEq<U<N>> for u128 {
    #[synth(inline)]
    #[inline]
    fn eq(&self, other: &U<N>) -> bool {
        other.eq(self)
    }
}

impl<const N: usize> PartialOrd<u128> for U<N> {
    fn partial_cmp(&self, other: &u128) -> Option<Ordering> {
        Some(self.cmp(&Self::cast_from(*other)))
    }

    #[synth(inline)]
    #[inline]
    fn lt(&self, other: &u128) -> bool {
        self.lt(&Self::cast_from(*other))
    }

    #[synth(inline)]
    #[inline]
    fn le(&self, other: &u128) -> bool {
        self.le(&Self::cast_from(*other))
    }

    #[synth(inline)]
    #[inline]
    fn gt(&self, other: &u128) -> bool {
        self.gt(&Self::cast_from(*other))
    }

    #[synth(inline)]
    #[inline]
    fn ge(&self, other: &u128) -> bool {
        self.ge(&Self::cast_from(*other))
    }
}

impl<const N: usize> PartialOrd<U<N>> for u128 {
    fn partial_cmp(&self, other: &U<N>) -> Option<Ordering> {
        other.partial_cmp(self)
    }

    #[synth(inline)]
    #[inline]
    fn lt(&self, other: &U<N>) -> bool {
        other.lt(self)
    }

    #[synth(inline)]
    #[inline]
    fn le(&self, other: &U<N>) -> bool {
        other.le(self)
    }

    #[synth(inline)]
    #[inline]
    fn gt(&self, other: &U<N>) -> bool {
        other.gt(self)
    }

    #[synth(inline)]
    #[inline]
    fn ge(&self, other: &U<N>) -> bool {
        other.ge(self)
    }
}

impl<const N: usize> CastFrom<Bit> for U<N> {
    #[synth(inline)]
    fn cast_from(from: Bit) -> Self {
        let v: U<1> = from.repack();
        v.cast()
    }
}

impl<const N: usize, const M: usize> CastFrom<U<M>> for U<N> {
    #[blackbox(CastFrom)]
    fn cast_from(from: U<M>) -> U<N> {
        match from {
            U::<M>::Short(short) => U::<N>::from_short(short),
            U::<M>::Long(long) => U::<N>::from_long(long),
        }
    }
}

macro_rules! impl_op {
    (impl $trait:ident ($method:ident) with $spec_method:ident) => {
        paste! {
            impl<const N: usize> $trait for U<N> {
                type Output = U<N>;

                #[blackbox([<Op $trait>])]
                fn $method(self, rhs: U<N>) -> Self::Output {
                    match (self, rhs) {
                        (U::Short(lhs), U::Short(rhs)) => {
                            U::from_short(lhs.$spec_method(rhs))
                        }
                        (U::Long(lhs), U::Long(rhs)) => {
                            U::from_long(lhs.$method(rhs))
                        }
                        _ => unreachable!(),
                    }
                }
            }

            impl<'a, const N: usize> $trait<U<N>> for &'a U<N> {
                type Output = U<N>;

                #[blackbox([<Op $trait>])]
                fn $method(self, rhs: U<N>) -> Self::Output {
                    match (self, rhs) {
                        (U::Short(lhs), U::Short(rhs)) => {
                            U::from_short((*lhs).$spec_method(rhs))
                        }
                        (U::Long(lhs), U::Long(rhs)) => {
                            U::from_long(lhs.$method(rhs))
                        }
                        _ => unreachable!(),
                    }
                }
            }

            impl<'a, const N: usize> $trait<&'a U<N>> for U<N> {
                type Output = U<N>;

                #[blackbox([<Op $trait>])]
                fn $method(self, rhs: &'a U<N>) -> Self::Output {
                    match (self, rhs) {
                        (U::Short(lhs), U::Short(rhs)) => {
                            U::from_short(lhs.$spec_method(*rhs))
                        }
                        (U::Long(lhs), U::Long(rhs)) => {
                            U::from_long(lhs.$method(rhs))
                        }
                        _ => unreachable!(),
                    }
                }
            }

            impl<'a, const N: usize> $trait<&'a BitVec<N>> for &'a BitVec<N> {
                type Output = BitVec<N>;

                #[blackbox([<Op $trait>])]
                fn $method(self, rhs: &'a BitVec<N>) -> Self::Output {
                    match (self, rhs) {
                        (U::Short(lhs), U::Short(rhs)) => {
                            U::from_short((*lhs).$spec_method(*rhs))
                        }
                        (U::Long(lhs), U::Long(rhs)) => {
                            U::from_long(lhs.$method(rhs))
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
    };
}

macro_rules! impl_ops_for_prim {
    (impl $trait:ident ($method:ident) for $($prim:ty),*) => {
        $(
            paste! {

                impl<const N: usize> $trait<$prim> for U<N> {
                    type Output = U<N>;

                    #[blackbox([<Op $trait>])]
                    fn $method(self, rhs: $prim) -> Self::Output {
                        self.$method(U::<N>::cast_from(rhs))
                    }
                }

                impl<'a, const N: usize> $trait<$prim> for &'a U<N> {
                    type Output = U<N>;

                    #[blackbox([<Op $trait>])]
                    fn $method(self, rhs: $prim) -> Self::Output {
                        self.$method(U::<N>::cast_from(rhs))
                    }
                }

                impl<const N: usize> $trait<U<N>> for $prim {
                    type Output = U<N>;

                    #[blackbox([<Op $trait>])]
                    fn $method(self, rhs: U<N>) -> Self::Output {
                        U::<N>::cast_from(self).$method(rhs)
                    }
                }

                impl<'a, const N: usize> $trait<&'a U<N>> for $prim {
                    type Output = U<N>;

                    #[blackbox([<Op $trait>])]
                    fn $method(self, rhs: &'a U<N>) -> Self::Output {
                        U::<N>::cast_from(self).$method(rhs)
                    }
                }
                }
        )*
    };
}

macro_rules! impl_ops {
    ($( impl $trait:ident ($method:ident) with $spec_method:ident ),+ $(,)?) => {
        $(
            impl_op!(impl $trait ($method) with $spec_method);
            impl_ops_for_prim!(impl $trait ($method) for u128);
        )+
    };
}

impl_ops!(
    impl BitAnd (bitand) with bitand,
    impl BitOr (bitor) with bitor,
    impl BitXor (bitxor) with bitxor,
    impl Add (add) with wrapping_add,
    impl Sub (sub) with wrapping_sub,
    impl Mul (mul) with wrapping_mul,
    impl Div (div) with wrapping_div,
    impl Rem (rem) with wrapping_rem
);

macro_rules! impl_shift_ops {
    ($( $prim:ty ),+) => {
        $(
            impl<const N: usize> Shl<$prim> for U<N> {
                type Output = U<N>;

                #[blackbox(OpShl)]
                fn shl(self, rhs: $prim) -> Self::Output {
                    match self {
                        Self::Short(short) => Self::from_short(short.shl(rhs)),
                        Self::Long(long) => Self::from_long(long.shl(rhs)),
                    }
                }
            }

            impl<'a, const N: usize> Shl<$prim> for &'a U<N> {
                type Output = U<N>;

                #[blackbox(OpShl)]
                fn shl(self, rhs: $prim) -> Self::Output {
                    match self {
                        U::Short(short) => U::from_short((*short).shl(rhs)),
                        U::Long(long) => U::from_long(long.shl(rhs)),
                    }
                }
            }

            impl<const N: usize> Shr<$prim> for U<N> {
                type Output = U<N>;

                #[blackbox(OpShr)]
                fn shr(self, rhs: $prim) -> Self::Output {
                    match self {
                        U::Short(short) => U::from_short(short.shr(rhs)),
                        U::Long(long) => U::from_long(long.shr(rhs)),
                    }
                }
            }

            impl<'a, const N: usize> Shr<$prim> for &'a U<N> {
                type Output = U<N>;

                #[blackbox(OpShr)]
                fn shr(self, rhs: $prim) -> Self::Output {
                    match self {
                        U::Short(short) => U::from_short((*short).shr(rhs)),
                        U::Long(long) => U::from_long(long.shr(rhs)),
                    }
                }
            }
        )+
    };
}

impl_shift_ops!(u8, u16, u32, u64, u128, usize);

impl<const N: usize, const M: usize> Shl<U<M>> for U<N>
where
    Assert<{ M <= 128 }>: IsTrue,
{
    type Output = Self;

    #[blackbox(OpShl)]
    fn shl(self, rhs: U<M>) -> Self::Output {
        let rhs: u128 = rhs.cast();
        self.shl(rhs)
    }
}

impl<const N: usize, const M: usize> Shr<U<M>> for U<N>
where
    Assert<{ M <= 128 }>: IsTrue,
{
    type Output = Self;

    #[blackbox(OpShr)]
    fn shr(self, rhs: U<M>) -> Self::Output {
        let rhs: u128 = rhs.cast();
        self.shr(rhs)
    }
}

impl<const N: usize> Not for U<N> {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Short(short) => Self::from_short(short.not()),
            Self::Long(long) => Self::from_long(BigUint::from_slice(
                long.iter_u32_digits()
                    .map(|digit| !digit)
                    .collect::<Vec<u32>>()
                    .as_slice(),
            )),
        }
    }
}

pub const fn in_range(n: usize, lo: usize, hi: usize) -> bool {
    assert!(lo < hi);
    n >= lo && n < hi
}

macro_rules! impl_trace_for_prims {
    ($( $prim:ty ),+) => {
        $(
            impl Traceable for $prim
            {
                fn add_vars(vars: &mut TraceVars) {
                    vars.add_ty(TraceTy::Bus(Self::BITS));
                }

                fn trace(
                    &self,
                    id: &mut IdCode,
                    tracer: &mut Tracer,
                ) -> io::Result<()> {
                    let mut v: Self = (1 << (Self::BITS - 1));
                    tracer.change_bus(
                        id,
                        (0 .. Self::BITS).map(|_| {
                            let value = bool_to_vcd((*self & v) > 0);
                            v >>= 1;
                            value
                        }),
                    )
                }
            }
        )+
    };
}

impl_trace_for_prims!(u128);

impl<const N: usize> Traceable for U<N> {
    fn add_vars(vars: &mut TraceVars) {
        vars.add_ty(TraceTy::Bus(N as u32));
    }

    fn trace(&self, id: &mut IdCode, tracer: &mut Tracer) -> io::Result<()> {
        let mut bv = (1_usize << (N - 1)).cast::<Self>();
        let zero = 0_u8.cast::<Self>();

        tracer.change_bus(
            id,
            (0 .. N).map(move |_| {
                let value = bool_to_vcd((self & &bv) > zero);
                bv = (&bv) >> 1_u8;
                value
            }),
        )
    }
}
