use std::{fs::File, io, path::Path};

use fhdl_netlist::{
    net_list::{ModuleId, NodeId},
    visitor::Visitor,
};
use rustc_const_eval::interpret::AllocId;
use rustc_data_structures::fx::{FxHashMap, FxIndexSet};
use rustc_middle::{
    mir::interpret,
    ty::{Interner, Ty, TyCtxt, TyEncoder},
};
use rustc_serialize::{opaque::FileEncoder, Encodable, Encoder};
use rustc_span::def_id::{CrateNum, DefId, DefIndex};
use rustc_type_ir::PredicateKind;

use crate::generator::Generator;

pub struct NetListEncoder<'g, 'tcx> {
    generator: &'g Generator<'tcx>,
    opaque: FileEncoder,
    type_shorthands: FxHashMap<Ty<'tcx>, usize>,
    predicate_shorthands: FxHashMap<PredicateKind<TyCtxt<'tcx>>, usize>,
    interpret_allocs: FxIndexSet<AllocId>,
}

impl<'g, 'tcx> NetListEncoder<'g, 'tcx> {
    pub fn new<P: AsRef<Path>>(
        generator: &'g Generator<'tcx>,
        path: P,
    ) -> io::Result<Self> {
        Ok(Self {
            generator,
            opaque: FileEncoder::new(path)?,
            type_shorthands: Default::default(),
            predicate_shorthands: Default::default(),
            interpret_allocs: Default::default(),
        })
    }

    pub fn encode_netlist(&mut self) {
        self.visit_modules();
    }

    pub fn encode<T: Encodable<Self>>(&mut self, value: T) -> usize {
        let pos = self.position();
        value.encode(self);
        pos
    }

    pub fn encode_interpret_allocs(&mut self) -> usize {
        let mut interpret_alloc_index = Vec::new();
        let mut n = 0;
        loop {
            let new_n = self.interpret_allocs.len();
            // if we have found new ids, serialize those, too
            if n == new_n {
                // otherwise, abort
                break;
            }
            for idx in n .. new_n {
                let id = self.interpret_allocs[idx];
                let pos = self.position() as u64;
                interpret_alloc_index.push(pos);
                interpret::specialized_encode_alloc_id(self, self.generator.tcx, id);
            }
            n = new_n;
        }

        self.encode(interpret_alloc_index)
    }

    pub fn flush(&mut self) {
        self.opaque.flush();
    }

    pub fn file(&self) -> &File {
        self.opaque.file()
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

impl<'g, 'tcx> Encoder for NetListEncoder<'g, 'tcx> {
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

impl<'g, 'tcx> TyEncoder for NetListEncoder<'g, 'tcx> {
    type I = TyCtxt<'tcx>;
    const CLEAR_CROSS_CRATE: bool = false;

    fn position(&self) -> usize {
        self.opaque.position()
    }

    fn type_shorthands(&mut self) -> &mut FxHashMap<<Self::I as Interner>::Ty, usize> {
        &mut self.type_shorthands
    }

    fn predicate_shorthands(&mut self) -> &mut FxHashMap<PredicateKind<Self::I>, usize> {
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

impl<'g, 'tcx> Encodable<NetListEncoder<'g, 'tcx>> for CrateNum {
    fn encode(&self, e: &mut NetListEncoder<'g, 'tcx>) {
        e.generator.tcx.stable_crate_id(*self).encode(e)
    }
}

impl<'g, 'tcx> Encodable<NetListEncoder<'g, 'tcx>> for DefId {
    fn encode(&self, e: &mut NetListEncoder<'g, 'tcx>) {
        e.generator.tcx.def_path_hash(*self).encode(e)
    }
}

impl<'g, 'tcx> Encodable<NetListEncoder<'g, 'tcx>> for DefIndex {
    fn encode(&self, _s: &mut NetListEncoder<'g, 'tcx>) {
        panic!("trying to encode `DefIndex` outside the context of a `DefId`")
    }
}

impl<'g, 'tcx> Visitor for NetListEncoder<'g, 'tcx> {
    fn visit_modules(&mut self) {
        for module_id in self.generator.net_list.modules() {
            if !self.generator.net_list[module_id].is_skip {
                self.visit_module(module_id);
            }
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        let module = &self.generator.net_list[module_id];
        module.encode(self);

        let mut cursor = self.generator.net_list.mod_cursor(module_id);
        while let Some(node_id) = self.generator.net_list.next(&mut cursor) {
            let node = &self.generator.net_list[node_id];
            if !node.is_skip {
                node.encode(self);
            }
        }
    }

    fn visit_node(&mut self, _: NodeId) {
        unreachable!()
    }
}
