use std::{
    cmp::Ordering::{self, *},
    fmt::{self, Binary, Display, LowerHex},
    io,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub},
};

use fhdl_macros::{blackbox, blackbox_ty, synth};
use paste::paste;
use vcd::IdCode;

use crate::{
    bitpack::{BitPack, BitSize},
    bitvec::BitVec,
    cast::{Cast, CastFrom},
    const_helpers::{Assert, ConstConstr, IsTrue},
    index::{idx_constr, Idx},
    signal::SignalValue,
    trace::{bool_to_vcd, TraceTy, TraceVars, Traceable, Tracer},
};

#[derive(Debug, Clone, Eq)]
#[blackbox_ty(Unsigned)]
#[repr(transparent)]
pub struct Unsigned<const N: usize>(BitVec<N>);

pub type U<const N: usize> = Unsigned<N>;

impl<const N: usize> PartialEq for Unsigned<N> {
    #[blackbox(OpEq)]
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }

    #[allow(clippy::partialeq_ne_impl)]
    #[blackbox(OpNe)]
    #[inline]
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl<const N: usize> PartialOrd for Unsigned<N> {
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

impl<const N: usize> Ord for Unsigned<N> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

macro_rules! impl_for_unsigned_prim_ty {
    ($( $prim:ty ),+) => {
        $(
            impl SignalValue for $prim {}

            impl <const N: usize> CastFrom<$prim> for Unsigned<N> {
                #[blackbox(CastFrom)]
                #[inline]
                fn cast_from(val: $prim) -> Self {
                    Self(val.cast())
                }
            }

            impl <const N: usize> CastFrom<Unsigned<N>> for $prim {
                #[blackbox(CastFrom)]
                #[inline]
                fn cast_from(val: Unsigned<N>) -> Self {
                    val.0.cast()
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

impl<const N: usize> PartialEq<u128> for Unsigned<N> {
    #[synth(inline)]
    #[inline]
    fn eq(&self, other: &u128) -> bool {
        self.eq(&Self::cast_from(*other))
    }
}

impl<const N: usize> PartialEq<Unsigned<N>> for u128 {
    #[synth(inline)]
    #[inline]
    fn eq(&self, other: &Unsigned<N>) -> bool {
        other.eq(self)
    }
}

impl<const N: usize> PartialOrd<u128> for Unsigned<N> {
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

impl<const N: usize> PartialOrd<Unsigned<N>> for u128 {
    fn partial_cmp(&self, other: &Unsigned<N>) -> Option<Ordering> {
        other.partial_cmp(self)
    }

    #[synth(inline)]
    #[inline]
    fn lt(&self, other: &Unsigned<N>) -> bool {
        other.lt(self)
    }

    #[synth(inline)]
    #[inline]
    fn le(&self, other: &Unsigned<N>) -> bool {
        other.le(self)
    }

    #[synth(inline)]
    #[inline]
    fn gt(&self, other: &Unsigned<N>) -> bool {
        other.gt(self)
    }

    #[synth(inline)]
    #[inline]
    fn ge(&self, other: &Unsigned<N>) -> bool {
        other.ge(self)
    }
}

impl<const N: usize> Unsigned<N> {
    #[synth(inline)]
    #[inline]
    pub fn idx<T>(&self, idx: T) -> bool
    where
        Idx<N>: CastFrom<T>,
        ConstConstr<{ idx_constr(N) }>:,
    {
        self.idx_(idx.cast())
    }

    #[blackbox(Index)]
    #[inline]
    fn idx_(&self, idx: Idx<N>) -> bool
    where
        ConstConstr<{ idx_constr(N) }>:,
    {
        self.0.idx(idx)
    }

    #[synth(inline)]
    pub fn rotate_left(self) -> Self {
        (self.clone() << 1) | ((self >> (N - 1)) & 1)
    }
}

impl<const N: usize> Default for Unsigned<N> {
    #[synth(inline)]
    #[inline]
    fn default() -> Self {
        0_u8.cast()
    }
}

impl<const N: usize> SignalValue for Unsigned<N> {}

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

impl<const N: usize, const M: usize> CastFrom<Unsigned<M>> for Unsigned<N> {
    fn cast_from(from: Unsigned<M>) -> Unsigned<N> {
        Self(from.0.cast())
    }
}

macro_rules! impl_op {
    ($trait:ident => $method:ident) => {
        paste! {

            impl<const N: usize> $trait for Unsigned<N> {
                type Output = Self;

                #[blackbox([<Op $trait>])]
                #[inline]
                fn $method(self, rhs: Self) -> Self::Output {
                    Self(self.0.$method(rhs.0))
                }
            }
        }
    };
}

macro_rules! impl_ops_for_prim {
    ( $trait:ident => $method:ident => $( $(#[$inner:meta])* $prim:ty ),+) => {
        paste! {

            $(
                impl<const N: usize> $trait<$prim> for Unsigned<N> {
                    type Output = Unsigned<N>;

                    #[blackbox([<Op $trait>])]
                    #[inline]
                    fn $method(self, rhs: $prim) -> Self::Output {
                        Unsigned::<N>(self.0.$method(rhs))
                    }
                }

                impl<const N: usize> $trait<Unsigned<N>> for $prim {
                    type Output = Unsigned<N>;

                    #[blackbox([<Op $trait>])]
                    #[inline]
                    fn $method(self, rhs: Unsigned<N>) -> Self::Output {
                        Unsigned::<N>(self.$method(rhs.0))
                    }
                }
            )+
        }
    };
}

macro_rules! impl_ops {
    ($( $trait:ident => $method:ident $( => $spec_method:ident )? ),+) => {
        $(
            impl_op!($trait => $method $( => $spec_method)?);

            impl_ops_for_prim!($trait => $method =>
                usize
            );
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

                #[blackbox(OpShl)]
                #[inline]
                fn shl(self, rhs: $prim) -> Self::Output {
                    Self(self.0.shl(rhs))
                }
            }

            impl<const N: usize> Shr<$prim> for Unsigned<N> {
                type Output = Self;

                #[blackbox(OpShr)]
                #[inline]
                fn shr(self, rhs: $prim) -> Self::Output {
                    Self(self.0.shr(rhs))
                }
            }
        )+
    };
}

impl_shift_ops!(usize);

impl<const N: usize> Shl<Unsigned<N>> for Unsigned<N>
where
    Assert<{ N <= 128 }>: IsTrue,
{
    type Output = Unsigned<N>;

    #[blackbox(OpShl)]
    #[inline]
    fn shl(self, rhs: Unsigned<N>) -> Self::Output {
        Self(self.0.shl(rhs.0))
    }
}

impl<const N: usize> Not for Unsigned<N> {
    type Output = Self;

    #[blackbox(OpNot)]
    #[inline]
    fn not(self) -> Self::Output {
        Self(self.0.not())
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

impl_trace_for_prims!(u8, u16, u32, u64, u128);

impl<const N: usize> Traceable for Unsigned<N> {
    fn add_vars(vars: &mut TraceVars) {
        BitVec::<N>::add_vars(vars);
    }

    #[inline]
    fn trace(&self, id: &mut IdCode, tracer: &mut Tracer) -> io::Result<()> {
        self.0.trace(id, tracer)
    }
}
