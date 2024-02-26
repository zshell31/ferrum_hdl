use strum::{Display, EnumString};

#[derive(Display, Debug, Clone, Copy, EnumString, PartialEq, Eq, Hash)]
pub enum BlackboxKind {
    ArrayChain,

    BitPackMsb,
    BitPackPack,
    BitPackUnpack,

    BitVecSlice,

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
    OpNot,

    CastFrom,

    Index,

    SignalAndThen,
    SignalApply2,
    SignalMap,
    SignalReg,
    SignalRegEn,
    SignalValue,
    SignalWatch,
    IntoSignal,

    StdClone,
}

#[derive(Display, Debug, Clone, Copy, EnumString, PartialEq, Eq, Hash)]
pub enum BlackboxTy {
    Signal,
    Wrapped,
    BitVec,
    Clock,
    Unsigned,
}
