use fhdl_macros::blackbox;

use crate::{
    bitpack::{BitPack, BitSize, IsPacked},
    bitvec::BitVec,
    cast::{Cast, CastFrom},
    const_helpers::{Assert, IsTrue},
    domain::ClockDomain,
    signal::{Bundle, Signal, SignalValue, Unbundle},
    simulation::{SimCtx, Simulate},
};

pub type Array<const N: usize, T> = [T; N];

impl<const N: usize, T: SignalValue> SignalValue for [T; N] {}

impl<const N: usize, T: BitSize> BitSize for [T; N] {
    const BITS: usize = N * T::BITS;
}

impl<const N: usize, T: SignalValue> BitPack for [T; N]
where
    T: BitPack<Packed = BitVec<{ T::BITS }>>,
    [(); <[T; N] as BitSize>::BITS]:,
{
    type Packed = BitVec<{ <[T; N] as BitSize>::BITS }>;

    fn pack(self) -> Self::Packed {
        let width = T::BITS;
        let mut bitvec = Self::Packed::zero();

        for item in self {
            bitvec = bitvec << width;
            bitvec = bitvec | item.pack().cast::<Self::Packed>();
        }

        bitvec
    }

    fn unpack(bitvec: Self::Packed) -> Self {
        let width = T::BITS;
        let mask: <T as BitPack>::Packed = ((1_usize << width) - 1_usize).into();
        let mut offset = (N - 1) * width;

        let vec = (0 .. N)
            .map(|_| {
                let slice = (bitvec.clone() >> offset).cast::<<T as BitPack>::Packed>()
                    & mask.clone();
                offset = offset.saturating_sub(width);
                T::unpack(slice)
            })
            .collect::<Vec<_>>();

        match <[T; N]>::try_from(vec) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        }
    }
}

fn transform_array<const N: usize, T, U>(a: [T; N], f: impl Fn(T) -> U) -> [U; N] {
    let v = a.into_iter().map(f).collect::<Vec<_>>();

    match <[U; N]>::try_from(v) {
        Ok(res) => res,
        Err(_) => unreachable!(),
    }
}

impl<const N: usize, T, U> CastFrom<[U; N]> for [T; N]
where
    T: CastFrom<U>,
{
    fn cast_from(from: [U; N]) -> [T; N] {
        transform_array(from, CastFrom::cast_from)
    }
}

pub trait ArrayExt<const N: usize, T> {
    fn at<const M: usize>(self) -> T
    where
        Assert<{ M < N }>: IsTrue,
        T: Clone;

    fn slice<const S: usize, const M: usize>(self) -> [T; M]
    where
        Assert<{ M > 0 }>: IsTrue,
        Assert<{ S + M - 1 < N }>: IsTrue,
        for<'a> [T; M]: TryFrom<&'a [T]>;

    fn reverse(self) -> Self
    where
        T: Clone;

    fn map<U>(self, f: impl Fn(T) -> U) -> [U; N];
}

impl<const N: usize, T> ArrayExt<N, T> for [T; N] {
    fn at<const M: usize>(self) -> T
    where
        Assert<{ M < N }>: IsTrue,
        T: Clone,
    {
        self[M].clone()
    }

    fn slice<const S: usize, const M: usize>(self) -> [T; M]
    where
        Assert<{ M > 0 }>: IsTrue,
        Assert<{ S + M - 1 < N }>: IsTrue,
        for<'a> [T; M]: TryFrom<&'a [T]>,
    {
        match <[T; M]>::try_from(&self[S .. (S + M)]) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        }
    }

    #[blackbox(ArrayReverse)]
    fn reverse(self) -> Self
    where
        T: Clone,
    {
        let mut values = self.clone();
        for i in 0 .. N {
            values[N - i - 1] = self[i].clone();
        }
        values
    }

    #[blackbox(ArrayMap)]
    fn map<U>(self, f: impl Fn(T) -> U) -> [U; N] {
        self.map(f)
    }
}

impl<const N: usize, D: ClockDomain, T: SignalValue> Unbundle for Signal<D, [T; N]> {
    type Unbundled = [Signal<D, T>; N];

    fn unbundle(self) -> Self::Unbundled {
        let signals = (0 .. N)
            .map(|ind| self.clone().map(move |s| s[ind].clone()))
            .collect::<Vec<_>>();

        match <[Signal<D, T>; N]>::try_from(signals) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        }
    }
}

impl<const N: usize, D: ClockDomain, T: SignalValue> Bundle for [Signal<D, T>; N] {
    type Bundled = Signal<D, [T; N]>;

    fn bundle(mut self) -> Self::Bundled {
        Signal::new(move |ctx| {
            let values = self
                .iter_mut()
                .map(|signal| signal.next(ctx))
                .collect::<Vec<_>>();

            match <[T; N]>::try_from(values) {
                Ok(res) => res,
                Err(_) => unreachable!(),
            }
        })
    }
}

impl<const N: usize, D: ClockDomain, T: SignalValue> Simulate for [Signal<D, T>; N] {
    type Value = [T; N];

    fn next(&mut self, ctx: &mut SimCtx) -> Self::Value {
        let values = (0 .. N).map(|ind| self[ind].next(ctx)).collect::<Vec<_>>();

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
        bitvec::BitVec,
        domain::TestSystem4,
        signal::SignalIterExt,
    };

    #[test]
    fn at() {
        assert_eq!([3, 2, 1, 0].at::<3>(), 0);
    }

    #[test]
    fn slice() {
        assert_eq!([3, 2, 1, 0].slice::<1, 2>(), [2, 1]);
    }

    #[test]
    fn unbundle() {
        let s = [[H, H, L], [L, H, L], [H, L, H], [L, L, H]]
            .into_iter()
            .into_signal::<TestSystem4>();

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
        let s = [
            [H, L, H, L].into_signal::<TestSystem4>(),
            [H, H, L, L].into_signal::<TestSystem4>(),
            [L, L, H, H].into_signal::<TestSystem4>(),
        ];

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
        let s: Array<3, Array<2, Bit>> = [[L, L], [H, H], [L, H]];

        assert_eq!(<Array<3, Array<2, Bit>> as BitSize>::BITS, 6);
        assert_eq!(s.pack(), BitVec::<6>::from(0b001101_u8));
    }

    #[test]
    fn unpack() {
        let b: BitVec<6> = BitVec::from(0b001101_u8);
        let s = Array::<3, Array<2, Bit>>::unpack(b);

        assert_eq!(s, [[L, L], [H, H], [L, H]]);
    }
}
