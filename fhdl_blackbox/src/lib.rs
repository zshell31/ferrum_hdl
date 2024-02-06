use strum::{Display, EnumString};

#[derive(Display, Debug, Clone, Copy, EnumString, PartialEq, Eq, Hash)]
pub enum BlackboxKind {
    ArrayMap,
    ArrayReverse,
    ArrayMake,

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

    Index,

    SignalAndThen,
    SignalApply2,
    SignalLift,
    SignalMap,
    SignalReg,
    SignalRegEn,
    SignalValue,
    SignalWatch,

    UnsignedBit,

    StdClone,
}

#[derive(Display, Debug, Clone, Copy, EnumString, PartialEq, Eq, Hash)]
pub enum BlackboxTy {
    Signal,
    Wrapped,
    BitVec,
    Clock,
    Unsigned,
    UnsignedShort,
}

impl BlackboxTy {
    pub fn is_unsigned_short(&self) -> bool {
        matches!(self, BlackboxTy::UnsignedShort)
    }

    pub fn has_constructor(&self) -> bool {
        self.is_unsigned_short()
    }
}
