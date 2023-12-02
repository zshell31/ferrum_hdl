use fhdl_macros::blackbox;

pub trait IsPrimTy: Sized {}

pub trait CastFrom<T: Sized>: Sized {
    fn cast_from(from: T) -> Self;
}

impl<U: IsPrimTy, T: From<U>> CastFrom<U> for T {
    fn cast_from(from: U) -> Self {
        Self::from(from)
    }
}

pub trait Cast: Sized {
    #[blackbox(Cast)]
    fn cast<T>(self) -> T
    where
        T: Sized + CastFrom<Self>,
    {
        CastFrom::cast_from(self)
    }
}

impl<T: Sized> Cast for T {}
