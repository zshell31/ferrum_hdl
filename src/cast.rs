use fhdl_macros::synth;

pub trait CastFrom<T: Sized>: Sized {
    fn cast_from(from: T) -> Self;
}

impl<'a, T, U> CastFrom<&'a T> for U
where
    T: Clone,
    U: CastFrom<T>,
{
    #[synth(inline)]
    #[inline]
    fn cast_from(from: &'a T) -> Self {
        let cast = Self::cast_from(from.clone());
        cast
    }
}

pub trait Cast: Sized {
    #[synth(inline)]
    #[inline]
    fn cast<T>(self) -> T
    where
        T: Sized + CastFrom<Self>,
    {
        let cast = CastFrom::cast_from(self);
        cast
    }
}

impl<T: Sized> Cast for T {}
