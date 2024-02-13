use fhdl_macros::{blackbox, synth};

pub trait IsPrimTy: Sized {}

pub trait CastFrom<T: Sized>: Sized {
    #[blackbox(CastFrom)]
    fn cast_from(from: T) -> Self;
}

pub trait Cast: Sized {
    #[synth(inline)]
    fn cast<T>(self) -> T
    where
        T: Sized + CastFrom<Self>,
    {
        CastFrom::cast_from(self)
    }
}

impl<T: Sized> Cast for T {}
