use std::io;

use fhdl_macros::blackbox;
use vcd::IdCode;

use crate::{
    bitpack::{BitPack, BitSize, BitVec},
    cast::{Cast, CastFrom},
    signal::SignalValue,
    trace::{bool_to_vcd, TraceTy, TraceVars, Traceable, Tracer},
};

pub type Bit = bool;

impl SignalValue for Bit {}

impl BitSize for Bit {
    const BITS: usize = 1;
}

impl CastFrom<Bit> for Bit {
    #[blackbox(CastFrom)]
    fn cast_from(from: Bit) -> Self {
        from
    }
}

impl BitPack for Bit {
    type Packed = BitVec<1>;

    fn pack(self) -> Self::Packed {
        (match self {
            true => 1_u8,
            false => 0,
        })
        .cast()
    }

    fn unpack(bitvec: Self::Packed) -> Self {
        bitvec.is_non_zero()
    }
}

pub const H: Bit = true;
pub const L: Bit = false;

impl Traceable for Bit {
    #[inline]
    fn add_vars(vars: &mut TraceVars) {
        vars.add_ty(TraceTy::Wire);
    }

    #[inline]
    fn trace(&self, id: &mut IdCode, tracer: &mut Tracer) -> io::Result<()> {
        tracer.change_wire(id, bool_to_vcd(*self))
    }
}
