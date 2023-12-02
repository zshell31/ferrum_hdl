use ferrum_macros::blackbox;
use ferrum_netlist::sig_ty::PrimTy;

pub trait IsPrimTy: Sized {
    const PRIM_TY: PrimTy;
}

impl IsPrimTy for bool {
    const PRIM_TY: PrimTy = PrimTy::Bool;
}

impl IsPrimTy for u128 {
    const PRIM_TY: PrimTy = PrimTy::U128;
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
