use fhdl_macros::blackbox;

pub trait IsPrimTy: Sized {}

#[fhdl_tool::cast_from]
pub trait CastFrom<T: Sized>: Sized {
    #[blackbox(CastFrom)]
    fn cast_from(from: T) -> Self;
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
