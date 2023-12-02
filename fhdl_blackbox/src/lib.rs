use strum::{Display, EnumString};

#[derive(Display, Debug, Clone, Copy, EnumString, PartialEq, Eq, Hash)]
pub enum BlackboxKind {
    ArrayMap,
    ArrayReverse,

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

    CastFrom,
    Cast,

    SignalAnd,
    SignalAndThen,
    SignalApply2,
    SignalEq,
    SignalLift,
    SignalMap,
    SignalOr,
    SignalReg,
    SignalRegEn,
    SignalReset,
    SignalValue,
    SignalWatch,

    UnsignedIndex,

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
