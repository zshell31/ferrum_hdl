use std::{fmt::Debug, ops::Index};

use crate::{
    const_asserts::{Assert, IsTrue},
    domain::ClockDomain,
    signal::{Bundle, MapSignal, Signal, SignalValue, Unbundle},
};

#[derive(Debug, Clone, Copy)]
pub struct Array<const N: usize, T>([T; N]);

impl<const N: usize, T: SignalValue> SignalValue for Array<N, T> {}

impl<const N: usize, T> From<[T; N]> for Array<N, T> {
    fn from(value: [T; N]) -> Self {
        Self(value)
    }
}

impl<const N: usize, T: PartialEq> PartialEq<Array<N, T>> for Array<N, T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<const N: usize, T: PartialEq> PartialEq<[T; N]> for Array<N, T> {
    fn eq(&self, other: &[T; N]) -> bool {
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

    // pub fn map()
}

impl<const N: usize, T> Index<usize> for Array<N, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

pub type UnbundledMap<
    const N: usize,
    T: SignalValue,
    D: ClockDomain,
    S: Signal<D, Value = Array<N, T>> + Clone,
> = Array<N, MapSignal<S, impl Fn(Array<N, T>) -> T + Clone>>;

impl<const N: usize, T, D> Unbundle<D, T> for Array<N, T>
where
    T: SignalValue,
    D: ClockDomain,
{
    type Bundled = Array<N, T>;
    type Unbundled = T;

    type UnbundledSig<S> = UnbundledMap<N, T, D, S>
    where
    S: Signal<D, Value = Self::Bundled> + Clone;

    fn unbundle<S>(signal: S) -> Self::UnbundledSig<S>
    where
        S: Signal<D, Value = Self::Bundled> + Clone,
    {
        let signals = (0 .. N)
            .map(|ind| signal.clone().smap(move |s| s[ind].clone()))
            .collect::<Vec<_>>();

        Array::from(match <[_; N]>::try_from(signals) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        })
    }
}

impl<const N: usize, T, D> Bundle<D, T> for Array<N, T>
where
    T: SignalValue,
    D: ClockDomain,
{
    type Bundled = Array<N, T>;
    type Unbundled = T;

    type UnbundledSig<S> = Array<N, S>
        where
            S: Signal<D, Value = Self::Unbundled>;

    fn bundle<S>(
        unbundled: Self::UnbundledSig<S>,
    ) -> impl Signal<D, Value = Self::Bundled>
    where
        S: Signal<D, Value = Self::Unbundled>,
    {
        unbundled
    }
}

impl<const N: usize, T: SignalValue, D: ClockDomain, S: Signal<D, Value = T>> Signal<D>
    for Array<N, S>
{
    type Value = Array<N, T>;

    fn next(&mut self) -> Self::Value {
        let values = self.0.iter_mut().map(|sig| sig.next()).collect::<Vec<_>>();
        Array::from(match <[_; N]>::try_from(values) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        })
    }
}

// impl<const N: usize, T, D> Bundle<D, T> for Array<N, T>
// where
//     T: SignalValue,
//     D: ClockDomain,
// {
//     type Unbundled<S> = Array<N, S>
//     where
//         S: Signal<D, Value = T> + Clone;

//     fn bundle<S>(unbundled: Self::Unbundled<S>) -> S
//     where
//         S: Signal<D, Value = T> + Clone,
//     {
//         todo!()
//     }
// }

// impl<const N: usize, T> Array<N, T> {
//     pub fn unbundle<D, S>(
//         signal: S,
//     ) -> Array<N, MapSignal<S, impl Fn(Array<N, T>) -> T + Clone>>
//     where
//         T: SignalValue,
//         D: ClockDomain,
//         S: Signal<D, Value = Array<N, T>> + Clone,
//     {
//         let signals = (0 .. N)
//             .map(|ind| signal.clone().smap(move |s| s[ind].clone()))
//             .collect::<Vec<_>>();

//         Array::from(match <[_; N]>::try_from(signals) {
//             Ok(res) => res,
//             Err(_) => unreachable!(),
//         })
//     }
// }

// impl<const N: usize, T, D, S> Array<N, S>
// where
//     T: SignalValue,
//     D: ClockDomain,
//     S: Signal<D, Value = T> + Clone,
// {
//     pub fn bundle(self) -> impl Signal<D, Value = Array<N, S>> {}
// }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        bit::{H, L},
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

        let res = Array::<3, _>::unbundle(s);

        let extract = |ind| res[ind].clone().iter().take(4).collect::<Vec<_>>();

        assert_eq!(extract(0), [H, L, H, L]);
        assert_eq!(extract(1), [H, H, L, L]);
        assert_eq!(extract(2), [L, L, H, H]);
    }

    #[test]
    fn bundle() {
        let s = Array::from([
            [H, L, H, L].into_signal::<TestSystem>(),
            [H, H, L, L].into_signal::<TestSystem>(),
            [L, L, H, H].into_signal::<TestSystem>(),
        ]);

        let res = Array::<3, _>::bundle(s);

        assert_eq!(res.iter().take(4).collect::<Vec<_>>(), [
            [H, H, L],
            [L, H, L],
            [H, L, H],
            [L, L, H]
        ]);
    }
}
