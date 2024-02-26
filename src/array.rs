use fhdl_macros::{blackbox, synth};
use smallvec::SmallVec;

use crate::{
    bitpack::{BitPack, BitSize, IsPacked},
    bitvec::BitVec,
    bundle::{Bundle, Unbundle},
    cast::{Cast, CastFrom},
    const_helpers::{Assert, ConstConstr, IsTrue},
    domain::ClockDomain,
    index::{idx_constr, Idx},
    signal::{Signal, SignalValue},
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
        let mask: <T as BitPack>::Packed = ((1_usize << width) - 1_usize).cast();
        let mut offset = (N - 1) * width;

        array_from_iter((0 .. N).map(|_| {
            let slice = (bitvec.clone() >> offset).cast::<<T as BitPack>::Packed>()
                & mask.clone();
            offset = offset.saturating_sub(width);
            T::unpack(slice)
        }))
    }
}

impl<const N: usize, T, U> CastFrom<[U; N]> for [T; N]
where
    U: Clone,
    T: CastFrom<U>,
    ConstConstr<{ idx_constr(N) }>:,
{
    #[synth(inline)]
    fn cast_from(from: [U; N]) -> [T; N] {
        from.map_(|from| T::cast_from(from))
    }
}

pub trait ArrayExt<const N: usize, T>: Sized {
    fn slice<const S: usize, const M: usize>(&self) -> [T; M]
    where
        Assert<{ M > 0 }>: IsTrue,
        Assert<{ S + M - 1 < N }>: IsTrue,
        for<'a> [T; M]: TryFrom<&'a [T]>;

    #[blackbox(Index)]
    fn idx(&self, idx: Idx<N>) -> T
    where
        ConstConstr<{ idx_constr(N) }>:,
        T: Clone;

    #[synth(inline)]
    fn reverse(self) -> [T; N]
    where
        T: Clone,
        ConstConstr<{ idx_constr(N) }>:,
    {
        <[T; N]>::chain_idx((), |idx, _| ((), self.idx(idx.rev()))).1
    }

    #[synth(inline)]
    fn map_<U>(self, f: impl Fn(T) -> U) -> [U; N]
    where
        T: Clone,
        ConstConstr<{ idx_constr(N) }>:,
    {
        <[U; N]>::chain_idx((), |idx, _| ((), f(self.idx(idx)))).1
    }

    #[synth(inline)]
    fn map_idx<U>(self, f: impl Fn(Idx<N>, T) -> U) -> [U; N]
    where
        T: Clone,
        ConstConstr<{ idx_constr(N) }>:,
    {
        <[U; N]>::chain_idx((), |idx, _| ((), f(idx.clone(), self.idx(idx)))).1
    }

    #[synth(inline)]
    fn make(f: impl Fn() -> T) -> [T; N]
    where
        ConstConstr<{ idx_constr(N) }>:,
    {
        <[T; N]>::chain_idx((), |_, _| ((), f())).1
    }

    #[synth(inline)]
    fn make_idx(f: impl Fn(Idx<N>) -> T) -> [T; N]
    where
        ConstConstr<{ idx_constr(N) }>:,
    {
        <[T; N]>::chain_idx((), |idx, _| ((), f(idx))).1
    }

    #[synth(inline)]
    fn chain<U>(init: U, f: impl Fn(U) -> (U, T)) -> (U, [T; N])
    where
        U: Clone,
        ConstConstr<{ idx_constr(N) }>:,
    {
        Self::chain_idx(init, |_, prev| f(prev))
    }

    #[blackbox(ArrayChain)]
    fn chain_idx<U>(init: U, f: impl Fn(Idx<N>, U) -> (U, T)) -> (U, [T; N])
    where
        U: Clone,
        ConstConstr<{ idx_constr(N) }>:;
}

impl<const N: usize, T> ArrayExt<N, T> for [T; N] {
    fn slice<const S: usize, const M: usize>(&self) -> [T; M]
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

    fn idx(&self, idx: Idx<N>) -> T
    where
        ConstConstr<{ idx_constr(N) }>:,
        T: Clone,
    {
        let idx = idx.val().cast::<usize>();
        self[idx].clone()
    }

    fn chain_idx<U>(init: U, f: impl Fn(Idx<N>, U) -> (U, T)) -> (U, [T; N])
    where
        U: Clone,
        ConstConstr<{ idx_constr(N) }>:,
    {
        let mut prev = init;
        let a = array_from_iter((0 .. N).map(|idx| {
            let (new_prev, item) = f(Idx::from_usize(idx), prev.clone());
            prev = new_prev;
            item
        }));

        (prev, a)
    }
}

impl<const N: usize, D: ClockDomain, T: SignalValue> Unbundle for Signal<D, [T; N]> {
    type Unbundled = [Signal<D, T>; N];

    fn unbundle(self) -> Self::Unbundled {
        array_from_iter((0 .. N).map(|ind| self.clone().map(move |s| s[ind].clone())))
    }
}

impl<const N: usize, D: ClockDomain, T: SignalValue> Bundle for [Signal<D, T>; N] {
    type Bundled = Signal<D, [T; N]>;

    fn bundle(mut self) -> Self::Bundled {
        Signal::new(move |ctx| {
            array_from_iter(self.iter_mut().map(|signal| signal.next(ctx)))
        })
    }
}

impl<const N: usize, D: ClockDomain, T: SignalValue> Simulate for [Signal<D, T>; N] {
    type Value = [T; N];

    fn next(&mut self, ctx: &mut SimCtx) -> Self::Value {
        array_from_iter((0 .. N).map(|ind| self[ind].next(ctx)))
    }
}

fn array_from_iter<T, const N: usize>(it: impl Iterator<Item = T>) -> Array<N, T> {
    let v = it.into_iter().collect::<SmallVec<[T; N]>>();
    assert_eq!(v.len(), N);

    match v.into_inner() {
        Ok(a) => a,
        Err(_) => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        bit::{Bit, H, L},
        bitvec::BitVec,
        domain::TD4,
        signal::SignalIterExt,
    };

    #[test]
    fn slice() {
        assert_eq!([3, 2, 1, 0].slice::<1, 2>(), [2, 1]);
    }

    #[test]
    fn unbundle() {
        let s = [[H, H, L], [L, H, L], [H, L, H], [L, L, H]]
            .into_iter()
            .into_signal::<TD4>();

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
            [H, L, H, L].into_signal::<TD4>(),
            [H, H, L, L].into_signal::<TD4>(),
            [L, L, H, H].into_signal::<TD4>(),
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
        assert_eq!(s.pack(), BitVec::<6>::cast_from(0b001101_u8));
    }

    #[test]
    fn unpack() {
        let b: BitVec<6> = BitVec::cast_from(0b001101_u8);
        let s = Array::<3, Array<2, Bit>>::unpack(b);

        assert_eq!(s, [[L, L], [H, H], [L, H]]);
    }

    #[test]
    fn array_idx() {
        let s: Array<4, u8> = [4, 3, 2, 1];
        let idx: Idx<4> = Default::default();

        assert_eq!(s.idx(idx.clone()), 4);
        let idx = idx.succ();
        assert_eq!(s.idx(idx.clone()), 3);
        let idx = idx.succ();
        assert_eq!(s.idx(idx.clone()), 2);
        let idx = idx.succ();
        assert_eq!(s.idx(idx.clone()), 1);
        let idx = idx.succ();
        assert_eq!(s.idx(idx.clone()), 4);
    }
}
