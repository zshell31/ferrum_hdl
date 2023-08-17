use std::ops::{BitAnd, BitOr, Not};

use crate::{bit::Bit, domain::DummySystem, signal::Clock};

#[derive(Debug, Clone, Copy)]
pub enum PrimTy {
    Bool,
    Bit,
    Clock,
}

impl PrimTy {
    pub fn is_bool(&self) -> bool {
        matches!(self, PrimTy::Bool)
    }

    pub fn width(&self) -> u8 {
        match self {
            Self::Bool => Bit::width(),
            Self::Bit => Bit::width(),
            Self::Clock => Clock::<DummySystem>::width(),
        }
    }
}

pub trait IsPrimTy {
    fn prim_ty() -> PrimTy;

    fn width() -> u8;
}

pub trait PrimValue: IsPrimTy {
    fn value(self) -> u128;
}

#[derive(Debug, Clone, Copy)]
pub struct DummyTy;

impl Not for DummyTy {
    type Output = Self;

    fn not(self) -> Self::Output {
        unreachable!()
    }
}

impl BitAnd for DummyTy {
    type Output = Self;

    fn bitand(self, _: Self) -> Self::Output {
        unreachable!()
    }
}

impl BitOr for DummyTy {
    type Output = Self;

    fn bitor(self, _: Self) -> Self::Output {
        unreachable!()
    }
}

impl IsPrimTy for DummyTy {
    fn prim_ty() -> PrimTy {
        unreachable!()
    }

    fn width() -> u8 {
        unreachable!()
    }
}

impl PrimValue for DummyTy {
    fn value(self) -> u128 {
        0
    }
}
