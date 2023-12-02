use fhdl_macros::blackbox;
use fhdl_netlist::sig_ty::PrimTy;

pub trait IsPrimTy: Sized {
    const PRIM_TY: PrimTy;
}

pub trait CastInner<T: Sized>: Sized {
    fn cast_inner(self) -> T;
}

impl<U: IsPrimTy, T: IsPrimTy + From<U>> CastInner<T> for U {
    fn cast_inner(self) -> T {
        T::from(self)
    }
}

pub trait Cast {
    #[blackbox(Cast)]
    fn cast<T>(self) -> T
    where
        Self: CastInner<T>,
    {
        CastInner::<T>::cast_inner(self)
    }
}

impl<T: Sized> Cast for T {}
