use std::{io, path::Path};

use fhdl_netlist::{
    net_list::{Module, ModuleId, NodeId, NodeOutId},
    symbol::Symbol,
};
use rustc_const_eval::interpret::AllocId;
use rustc_data_structures::fx::{FxHashMap, FxIndexSet};
use rustc_hir::def_id::CrateNum;
use rustc_middle::ty::{PredicateKind, Ty, TyCtxt, TyEncoder};
use rustc_serialize::{opaque::FileEncoder, Encodable, Encoder};
use rustc_span::def_id::DefIndex;

pub struct NetListEncoder<'tcx> {
    tcx: TyCtxt<'tcx>,
    opaque: FileEncoder,
    type_shorthands: FxHashMap<Ty<'tcx>, usize>,
    predicate_shorthands: FxHashMap<PredicateKind<'tcx>, usize>,
    interpret_allocs: FxIndexSet<AllocId>,
}

impl<'tcx> NetListEncoder<'tcx> {
    pub fn new<P: AsRef<Path>>(tcx: TyCtxt<'tcx>, path: P) -> io::Result<Self> {
        Ok(Self {
            tcx,
            opaque: FileEncoder::new(path)?,
            type_shorthands: Default::default(),
            predicate_shorthands: Default::default(),
            interpret_allocs: Default::default(),
        })
    }
}

macro_rules! encoder_methods {
    ($($name:ident($ty:ty);)*) => {
        #[inline]
        $(fn $name(&mut self, value: $ty) {
            self.opaque.$name(value)
        })*
    }
}

impl<'tcx> Encoder for NetListEncoder<'tcx> {
    encoder_methods! {
        emit_usize(usize);
        emit_u128(u128);
        emit_u64(u64);
        emit_u32(u32);
        emit_u16(u16);
        emit_u8(u8);

        emit_isize(isize);
        emit_i128(i128);
        emit_i64(i64);
        emit_i32(i32);
        emit_i16(i16);

        emit_raw_bytes(&[u8]);
    }
}

impl<'tcx> TyEncoder for NetListEncoder<'tcx> {
    type I = TyCtxt<'tcx>;
    const CLEAR_CROSS_CRATE: bool = false;

    fn position(&self) -> usize {
        self.opaque.position()
    }

    fn type_shorthands(
        &mut self,
    ) -> &mut FxHashMap<<Self::I as rustc_type_ir::Interner>::Ty, usize> {
        &mut self.type_shorthands
    }

    fn predicate_shorthands(
        &mut self,
    ) -> &mut FxHashMap<<Self::I as rustc_type_ir::Interner>::PredicateKind, usize> {
        &mut self.predicate_shorthands
    }

    fn encode_alloc_id(
        &mut self,
        alloc_id: &<Self::I as rustc_type_ir::Interner>::AllocId,
    ) {
        let (index, _) = self.interpret_allocs.insert_full(*alloc_id);
        index.encode(self);
    }
}

impl<'tcx> Encodable<NetListEncoder<'tcx>> for CrateNum {
    fn encode(&self, e: &mut NetListEncoder<'tcx>) {
        e.emit_u32(self.as_u32())
    }
}

impl<'tcx> Encodable<NetListEncoder<'tcx>> for DefIndex {
    fn encode(&self, e: &mut NetListEncoder<'tcx>) {
        e.emit_u32(self.as_u32())
    }
}

// impl<'tcx> Encodable<NetListEncoder<'tcx>> for ModuleId {
//     fn encode(&self, e: &mut NetListEncoder<'tcx>) {
//         e.emit_u32(self.as_u32())
//     }
// }

// impl<'tcx> Encodable<NetListEncoder<'tcx>> for NodeId {
//     fn encode(&self, e: &mut NetListEncoder<'tcx>) {
//         self.as_u32_tuple().encode(e);
//     }
// }

// impl<'tcx> Encodable<NetListEncoder<'tcx>> for NodeOutId {
//     fn encode(&self, e: &mut NetListEncoder<'tcx>) {
//         self.as_u32_tuple().encode(e);
//     }
// }

// impl<'tcx> Encodable<NetListEncoder<'tcx>> for Symbol {
//     fn encode(&self, e: &mut NetListEncoder<'tcx>) {
//         self.as_str().encode(e);
//     }
// }

// impl<'tcx> Encodable<NetListEncoder<'tcx>> for Module {
//     fn encode(&self, e: &mut NetListEncoder<'tcx>) {}
// }
