use strum::{Display, EnumString};

#[derive(Display, Debug, Clone, Copy, EnumString, PartialEq, Eq, Hash)]
pub enum BlackboxKind {
    ArrayChain,

    BitPackPack,
    BitPackUnpack,

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
    Slice,

    RegEn,
    RegEnComb,

    SignalAndThen,
    SignalApply2,
    SignalDff,
    SignalDffComb,
    SignalMap,
    SignalValue,
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
    UnsignedInner,
    Signed,
    Reg,
}
