use core::fmt;
use std::{
    fmt::{Binary, Debug, Display, LowerHex},
    ops::Index,
};

use fhdl_macros::{blackbox, blackbox_ty};
use smallvec::SmallVec;

use crate::{
    bit_pack::{BitPack, BitSize},
    bit_vec::{BitVec, BitVecInner},
    cast::CastInner,
    const_helpers::{Assert, IsTrue},
    domain::ClockDomain,
    signal::{Bundle, Signal, SignalValue, Unbundle},
    simulation::{SimCtx, Simulate},
};

#[derive(Clone, Copy)]
#[blackbox_ty(Array)]
#[repr(transparent)]
pub struct Array<const N: usize, T>([T; N]);

impl<const N: usize, T: SignalValue> SignalValue for Array<N, T> {}

impl<const N: usize, T: SignalValue> SignalValue for [T; N] {}

impl<const N: usize, T: SignalValue + Display> Display for Array<N, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<const N: usize, T: SignalValue + Debug> Debug for Array<N, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<const N: usize, T: SignalValue + Binary> Binary for Array<N, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<const N: usize, T: SignalValue + LowerHex> LowerHex for Array<N, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<const N: usize, T: BitSize> BitSize for Array<N, T> {
    const BITS: usize = N * T::BITS;
}

impl<const N: usize, T: BitSize> BitSize for [T; N] {
    const BITS: usize = N * T::BITS;
}

impl<const N: usize, T: SignalValue> BitPack for [T; N]
where
    BitVec<{ N * T::BITS }>:,
    BitVec<{ T::BITS }>:,
    T: BitPack<Packed = BitVec<{ T::BITS }>>,
{
    type Packed = BitVec<{ N * T::BITS }>;

    fn pack(&self) -> Self::Packed {
        let width = T::BITS;
        assert!(width * N <= 128);

        let mut bitvec: BitVecInner = 0;

        for item in self {
            bitvec <<= width;
            bitvec |= item.pack().inner();
        }

        bitvec.into()
    }

    fn unpack(bitvec: Self::Packed) -> Self {
        let width = T::BITS;
        assert!(width * N <= 128);
        let mask = (1 << width) - 1;
        let mut offset = (N - 1) * width;

        let vec = (0 .. N)
            .map(|_| {
                let slice =
                    BitVec::<{ T::BITS }>::from((bitvec.inner() >> offset) & mask);
                offset = offset.saturating_sub(width);
                T::unpack(slice)
            })
            .collect::<SmallVec<[T; 8]>>();

        match <[T; N]>::try_from(vec.as_slice()) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        }
    }
}

impl<const N: usize, T: SignalValue> BitPack for Array<N, T>
where
    BitVec<{ N * T::BITS }>:,
    BitVec<{ T::BITS }>:,
    T: BitPack<Packed = BitVec<{ T::BITS }>>,
{
    type Packed = BitVec<{ N * T::BITS }>;

    fn pack(&self) -> Self::Packed {
        self.0.pack()
    }

    fn unpack(bitvec: Self::Packed) -> Self {
        Self(<[T; N]>::unpack(bitvec))
    }
}

impl<const N: usize, T> From<[T; N]> for Array<N, T> {
    fn from(value: [T; N]) -> Self {
        Self(value)
    }
}

impl<const N: usize, T> From<Array<N, T>> for [T; N] {
    fn from(value: Array<N, T>) -> Self {
        value.0
    }
}

fn transform_array<const N: usize, T, U>(a: [T; N], f: impl Fn(T) -> U) -> [U; N] {
    let v = a.into_iter().map(f).collect::<Vec<_>>();

    match <[U; N]>::try_from(v) {
        Ok(res) => res,
        Err(_) => unreachable!(),
    }
}

impl<const N: usize, T, U> CastInner<[U; N]> for [T; N]
where
    T: CastInner<U>,
{
    fn cast_inner(self) -> [U; N] {
        transform_array(self, CastInner::cast_inner)
    }
}

impl<const N: usize, T, U> CastInner<[U; N]> for Array<N, T>
where
    T: CastInner<U>,
{
    fn cast_inner(self) -> [U; N] {
        transform_array(self.0, CastInner::cast_inner)
    }
}

impl<const N: usize, T, U> CastInner<Array<N, T>> for [U; N]
where
    U: CastInner<T>,
{
    fn cast_inner(self) -> Array<N, T> {
        transform_array(self, CastInner::cast_inner).into()
    }
}

impl<const N: usize, T, U> CastInner<Array<N, T>> for Array<N, U>
where
    U: CastInner<T>,
{
    fn cast_inner(self) -> Array<N, T> {
        transform_array(self.0, CastInner::cast_inner).into()
    }
}

impl<const N: usize, T: PartialEq> PartialEq<Array<N, T>> for Array<N, T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<const N: usize, U, T: PartialEq<U>> PartialEq<[U; N]> for Array<N, T> {
    fn eq(&self, other: &[U; N]) -> bool {
        self.0 == *other
    }
}

impl<const N: usize, T: Eq> Eq for Array<N, T> {}

impl<const N: usize, T> Array<N, T> {
    pub fn at<const M: usize>(self) -> T
    where
        Assert<{ M < N }>: IsTrue,
        T: Clone,
    {
        self.0[M].clone()
    }

    pub fn slice<const S: usize, const M: usize>(self) -> Array<M, T>
    where
        Assert<{ M > 0 }>: IsTrue,
        Assert<{ S + M - 1 < N }>: IsTrue,
        for<'a> [T; M]: TryFrom<&'a [T]>,
    {
        Array::from(match <[T; M]>::try_from(&self.0[S .. (S + M)]) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        })
    }

    #[blackbox(ArrayReverse)]
    pub fn reverse(self) -> Self
    where
        T: Clone,
    {
        let mut values = self.0.clone();
        for i in 0 .. N {
            values[N - i - 1] = self.0[i].clone();
        }
        values.into()
    }

    #[blackbox(ArrayMap)]
    pub fn map<U>(self, f: impl Fn(T) -> U) -> Array<N, U> {
        self.0.map(f).into()
    }
}

impl<const N: usize, T> Index<usize> for Array<N, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<const N: usize, D: ClockDomain, T: SignalValue> Unbundle for Signal<D, Array<N, T>> {
    type Unbundled = Array<N, Signal<D, T>>;

    fn unbundle(self) -> Self::Unbundled {
        let signals = (0 .. N)
            .map(|ind| self.clone().map(move |s| s[ind]))
            .collect::<Vec<_>>();

        Array::from(match <[Signal<D, T>; N]>::try_from(signals) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        })
    }
}

impl<const N: usize, D: ClockDomain, T: SignalValue> Bundle for Array<N, Signal<D, T>> {
    type Bundled = Signal<D, Array<N, T>>;

    fn bundle(mut self) -> Self::Bundled {
        Signal::new(move |ctx| {
            let values = self
                .0
                .iter_mut()
                .map(|signal| signal.next(ctx))
                .collect::<Vec<_>>();

            Array::from(match <[T; N]>::try_from(values) {
                Ok(res) => res,
                Err(_) => unreachable!(),
            })
        })
    }
}

impl<const N: usize, D: ClockDomain, T: SignalValue> Simulate for Array<N, Signal<D, T>> {
    type Value = [T; N];

    fn next(&mut self, ctx: &mut SimCtx) -> Self::Value {
        let values = (0 .. N)
            .map(|ind| self.0[ind].next(ctx))
            .collect::<Vec<_>>();

        match <[_; N]>::try_from(values) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        bit::{Bit, H, L},
        bit_vec::BitVec,
        cast::Cast,
        signal::SignalIterExt,
    };

    pub struct TestSystem;

    impl ClockDomain for TestSystem {
        const FREQ: usize = 4;
    }

    #[test]
    fn at() {
        assert_eq!(Array::from([3, 2, 1, 0]).at::<3>(), 0);
    }

    #[test]
    fn slice() {
        assert_eq!(
            Array::from([3, 2, 1, 0]).slice::<1, 2>(),
            Array::from([2, 1])
        );
    }

    #[test]
    fn unbundle() {
        let s = [[H, H, L], [L, H, L], [H, L, H], [L, L, H]]
            .into_iter()
            .map(Array::from)
            .into_signal::<TestSystem>();

        let res = s.unbundle();

        assert_eq!(res.simulate().take(4).collect::<Vec<_>>(), [
            [H, H, L],
            [L, H, L],
            [H, L, H],
            [L, L, H]
        ]);
    }

    #[test]
    fn bundle() {
        let s = Array::from([
            [H, L, H, L].into_signal::<TestSystem>(),
            [H, H, L, L].into_signal::<TestSystem>(),
            [L, L, H, H].into_signal::<TestSystem>(),
        ]);

        let res = s.bundle();

        assert_eq!(res.simulate().take(4).collect::<Vec<_>>(), [
            [H, H, L],
            [L, H, L],
            [H, L, H],
            [L, L, H]
        ]);
    }

    #[test]
    fn pack() {
        let s: Array<3, Array<2, Bit>> = [[L, L], [H, H], [L, H]].cast_inner();

        assert_eq!(<Array<3, Array<2, Bit>> as BitSize>::BITS, 6);
        assert_eq!(s.pack(), BitVec::<6>::from(0b001101));
    }

    #[test]
    fn unpack() {
        let b: BitVec<6> = BitVec::from(0b001101);
        let s = Array::<3, Array<2, Bit>>::unpack(b);

        assert_eq!(
            s,
            [[L, L], [H, H], [L, H]].cast::<Array<3, Array<2, Bit>>>()
        );
    }
}
