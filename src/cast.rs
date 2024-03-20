use fhdl_macros::synth;

pub trait CastFrom<T: Sized>: Sized {
    fn cast_from(from: T) -> Self;
}

pub trait Cast: Sized {
    #[synth(inline)]
    #[inline]
    fn cast<T>(self) -> T
    where
        T: Sized + CastFrom<Self>,
    {
        CastFrom::cast_from(self)
    }
}

impl<T: Sized> Cast for T {}
