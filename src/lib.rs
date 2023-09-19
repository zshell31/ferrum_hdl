#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(register_tool)]
#![feature(type_alias_impl_trait)]
#![feature(cell_update)]
#![feature(const_trait_impl)]
#![register_tool(ferrum_tool)]

pub mod array;
pub mod bit;
pub mod bit_pack;
pub mod bit_vec;
pub mod blackbox;
pub mod const_asserts;
pub mod const_functions;
pub mod domain;
pub mod signal;
pub mod signal_fn;
pub mod simulation;
pub mod unsigned;

pub trait CastInner<T: Sized>: Sized {
    fn cast_inner(self) -> T;
}

impl<T: Sized> CastInner<T> for T {
    fn cast_inner(self) -> T {
        self
    }
}

pub trait Cast {
    fn cast<T>(self) -> T
    where
        Self: CastInner<T>,
    {
        CastInner::<T>::cast_inner(self)
    }
}

impl<T: Sized> Cast for T {}
