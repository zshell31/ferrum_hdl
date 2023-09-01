use crate::{
    const_asserts::{Assert, IsTrue},
    signal::SignalValue,
};

#[derive(Debug, Clone, Copy)]
pub struct Array<const N: usize, T: Copy>([T; N]);

impl<const N: usize, T: Copy + SignalValue> SignalValue for Array<N, T> {}

impl<const N: usize, T: Copy> From<[T; N]> for Array<N, T> {
    fn from(value: [T; N]) -> Self {
        Self(value)
    }
}

impl<const N: usize, T: Copy + PartialEq> PartialEq for Array<N, T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<const N: usize, T: Copy + Eq> Eq for Array<N, T> {}

impl<const N: usize, T: Copy> Array<N, T> {
    pub fn at<const M: usize>(self) -> T
    where
        Assert<{ M < N }>: IsTrue,
    {
        self.0[M]
    }

    pub fn slice<const S: usize, const M: usize>(self) -> Array<M, T>
    where
        Assert<{ M > 0 }>: IsTrue,
        Assert<{ S + M - 1 < N }>: IsTrue,
    {
        Array::from(<[T; M]>::try_from(&self.0[S .. (S + M)]).unwrap())
    }

    pub fn reverse(self) -> Self {
        let mut values = self.0;
        for i in 0 .. N {
            values[N - i - 1] = self.0[i];
        }
        values.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
