use std::{
    cmp::Ordering,
    fmt::{self, Binary, Display, LowerHex},
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub},
};

use fhdl_macros::{blackbox, blackbox_ty};
use fhdl_netlist::sig_ty::PrimTy;

use crate::{
    bitpack::{BitPack, BitSize},
    bitvec::BitVec,
    cast::IsPrimTy,
    const_functions::bit,
    const_helpers::{Assert, IsTrue},
    signal::SignalValue,
};

pub fn unsigned_value(val: u128, width: u128) -> u128 {
    if width == 128 {
        val
    } else {
        val & ((1 << width) - 1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[blackbox_ty(Unsigned)]
#[repr(transparent)]
pub struct Unsigned<const N: usize>(BitVec<N>);

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[blackbox_ty(UnsignedShort)]
#[repr(transparent)]
pub struct u<const N: usize>(pub u128)
where
    Assert<{ N <= 128 }>: IsTrue;

macro_rules! impl_is_prim_ty {
    ($( $prim:ty => $prim_ty:ident ),+) => {
        $(
            impl IsPrimTy for $prim {
                const PRIM_TY: PrimTy = PrimTy::$prim_ty;
            }
        )+
    };
}

impl_is_prim_ty!(
    u8 => U8,
    u16 => U16,
    u32 => U32,
    u64 => U64,
    u128 => U128
);

impl<const N: usize> IsPrimTy for u<N>
where
    Assert<{ N <= 128 }>: IsTrue,
{
    const PRIM_TY: PrimTy = PrimTy::Unsigned(N as u128);
}

impl<const N: usize> Unsigned<N> {
    #[blackbox(UnsignedIndex)]
    pub fn bit<const M: usize>(self) -> bool
    where
        Assert<{ bit(M, N) }>: IsTrue,
    {
        self.0.bit::<M>()
    }
}

impl<const N: usize> SignalValue for Unsigned<N> {}

impl<const N: usize> IsPrimTy for Unsigned<N> {
    const PRIM_TY: PrimTy = PrimTy::Unsigned(N as u128);
}

impl<const N: usize> BitSize for Unsigned<N> {
    const BITS: usize = N;
}

impl<const N: usize> BitPack for Unsigned<N> {
    type Packed = BitVec<N>;

    fn pack(self) -> Self::Packed {
        self.0.clone()
    }

    fn unpack(bitvec: Self::Packed) -> Self {
        Self(bitvec)
    }
}

impl<const N: usize> Display for Unsigned<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "u({})", self.0)
    }
}

impl<const N: usize> Binary for Unsigned<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "u({:b})", self.0)
    }
}

impl<const N: usize> LowerHex for Unsigned<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "u({:x})", self.0)
    }
}

macro_rules! impl_from {
    ($( $prim:ty => $prim_bits:literal ),+) => {
        $(
            impl <const N: usize> From<$prim> for Unsigned<N> {
                #[inline(always)]
                fn from(val: $prim) -> Self {
                    Self(val.into())
                }
            }

            impl <const N: usize> From<Unsigned<N>> for $prim
            where
                Assert<{ N <= $prim_bits }>: IsTrue
            {
                #[inline(always)]
                fn from(val: Unsigned<N>) -> Self {
                    val.0.into()
                }
            }
        )+
    };
}

impl_from!(
    u8 => 8,
    u16 => 16,
    u32 => 32,
    u64 => 64,
    u128 => 128,
    usize => 64
);

impl<const N: usize> From<u<N>> for Unsigned<N>
where
    Assert<{ N <= 128 }>: IsTrue,
{
    fn from(value: u<N>) -> Self {
        Self(value.0.into())
    }
}

impl<const N: usize> From<Unsigned<N>> for u<N>
where
    Assert<{ N <= 128 }>: IsTrue,
{
    fn from(value: Unsigned<N>) -> Self {
        Self(value.0.into())
    }
}

impl<const N: usize> PartialEq<u128> for Unsigned<N> {
    fn eq(&self, other: &u128) -> bool {
        self.eq(&Self::from(*other))
    }
}

impl<const N: usize> PartialOrd<u128> for Unsigned<N> {
    fn partial_cmp(&self, other: &u128) -> Option<Ordering> {
        Some(self.cmp(&Self::from(*other)))
    }
}

macro_rules! impl_op {
    ($trait:ident => $method:ident) => {
        impl<const N: usize> $trait for Unsigned<N> {
            type Output = Self;

            fn $method(self, rhs: Self) -> Self::Output {
                Self(self.0.$method(rhs.0))
            }
        }
    };
}

macro_rules! impl_ops_for_prim {
    ($trait:ident => $method:ident => $( $prim:ty ),+) => {
        $(
            impl<const N: usize> $trait<$prim> for Unsigned<N> {
                type Output = Unsigned<N>;

                fn $method(self, rhs: $prim) -> Self::Output {
                    Unsigned::<N>(self.0.$method(rhs))
                }
            }

            impl<const N: usize> $trait<Unsigned<N>> for $prim {
                type Output = Unsigned<N>;

                fn $method(self, rhs: Unsigned<N>) -> Self::Output {
                    Unsigned::<N>(self.$method(rhs.0))
                }
            }
        )+
    };
}

macro_rules! impl_ops {
    ($( $trait:ident => $method:ident $( => $spec_method:ident )? ),+) => {
        $(
            impl_op!($trait => $method $( => $spec_method)?);

            impl_ops_for_prim!($trait => $method => u8, u16, u32, u64, u128, usize);
        )+
    };
}

impl_ops!(
    BitAnd => bitand,
    BitOr => bitor,
    BitXor => bitxor,
    Add => add,
    Sub => sub,
    Mul => mul,
    Div => div,
    Rem => rem
);

macro_rules! impl_shift_ops {
    ($( $prim:ty ),+) => {
        $(
            impl<const N: usize> Shl<$prim> for Unsigned<N> {
                type Output = Self;

                fn shl(self, rhs: $prim) -> Self::Output {
                    Self(self.0.shl(rhs))
                }
            }

            impl<const N: usize> Shr<$prim> for Unsigned<N> {
                type Output = Self;

                fn shr(self, rhs: $prim) -> Self::Output {
                    Self(self.0.shr(rhs))
                }
            }
        )+
    };
}

impl_shift_ops!(u8, u16, u32, u64, u128, usize);

impl<const N: usize> Not for Unsigned<N> {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(self.0.not())
    }
}
