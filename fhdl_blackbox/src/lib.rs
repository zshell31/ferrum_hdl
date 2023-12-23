#![cfg_attr(feature = "encoding", feature(rustc_private))]

#[cfg(feature = "encoding")]
extern crate rustc_macros;
#[cfg(feature = "encoding")]
extern crate rustc_serialize;

#[cfg(feature = "encoding")]
use rustc_macros::{Decodable, Encodable};
use strum::{Display, EnumString};

#[derive(Display, Debug, Clone, Copy, EnumString, PartialEq, Eq, Hash)]
pub enum BlackboxKind {
    ArrayMap,
    ArrayReverse,
    ArrayMake,

    BitH,
    BitL,

    BitPackMsb,
    BitPackPack,
    BitPackRepack,

    BitVecShrink,
    BitVecSlice,
    BitVecUnpack,

    Bundle,
    Unbundle,

    OpEq,
    OpNe,
    OpLt,
    OpLe,
    OpGt,
    OpGe,
    OpBitAnd,
    OpBitOr,
    OpBitXor,
    OpAnd,
    OpOr,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpRem,
    OpShl,
    OpShr,

    CastFrom,
    Cast,

    Index,

    SignalAnd,
    SignalAndThen,
    SignalApply2,
    SignalEq,
    SignalLift,
    SignalMap,
    SignalOr,
    SignalReg,
    SignalRegEn,
    SignalValue,
    SignalWatch,

    UnsignedBit,

    StdClone,
}

impl BlackboxKind {
    pub fn is_cast(&self) -> Option<bool> {
        match self {
            Self::CastFrom => Some(true),
            Self::Cast => Some(false),
            _ => None,
        }
    }
}

#[derive(Display, Debug, Clone, Copy, EnumString, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "encoding", derive(Encodable, Decodable))]
pub enum BlackboxTy {
    Signal,
    Wrapped,
    BitVec,
    Bit,
    Clock,
    Unsigned,
    UnsignedShort,
    Array,
}

impl BlackboxTy {
    pub fn is_unsigned_short(&self) -> bool {
        matches!(self, BlackboxTy::UnsignedShort)
    }

    pub fn has_constructor(&self) -> bool {
        self.is_unsigned_short()
    }
}
