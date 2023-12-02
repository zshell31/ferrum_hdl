use std::mem;

use rustc_const_eval::interpret::AllocDecodingSession;
use rustc_data_structures::fx::FxHashMap;
use rustc_middle::{
    implement_ty_decoder,
    ty::{Ty, TyCtxt, TyDecoder},
};
use rustc_serialize::{opaque::MemDecoder, Decodable};
use rustc_session::StableCrateId;
use rustc_span::def_id::{CrateNum, DefId, DefIndex, DefPathHash};

use crate::generator::Generator;

pub struct NetListDecoder<'g, 'tcx> {
    generator: &'g mut Generator<'tcx>,
    opaque: MemDecoder<'g>,
    alloc_decoding_session: AllocDecodingSession<'g>,
    type_shorthands: FxHashMap<usize, Ty<'tcx>>,
}

implement_ty_decoder!(NetListDecoder<'g, 'tcx>);

impl<'g, 'tcx> TyDecoder for NetListDecoder<'g, 'tcx> {
    type I = TyCtxt<'tcx>;
    const CLEAR_CROSS_CRATE: bool = false;

    fn interner(&self) -> Self::I {
        self.generator.tcx
    }

    fn cached_ty_for_shorthand<F>(
        &mut self,
        shorthand: usize,
        or_insert_with: F,
    ) -> <Self::I as rustc_type_ir::Interner>::Ty
    where
        F: FnOnce(&mut Self) -> <Self::I as rustc_type_ir::Interner>::Ty,
    {
        match self.type_shorthands.get(&shorthand) {
            Some(ty) => *ty,
            None => {
                let ty = or_insert_with(self);
                self.type_shorthands.insert(shorthand, ty);
                ty
            }
        }
    }

    fn with_position<F, R>(&mut self, pos: usize, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let new_opaque = MemDecoder::new(self.opaque.data(), pos);
        let old_opaque = mem::replace(&mut self.opaque, new_opaque);
        let r = f(self);
        self.opaque = old_opaque;
        r
    }

    fn decode_alloc_id(&mut self) -> <Self::I as rustc_type_ir::Interner>::AllocId {
        let alloc_decoding_session = self.alloc_decoding_session;
        alloc_decoding_session.decode_alloc_id(self)
    }
}

impl<'g, 'tcx> Decodable<NetListDecoder<'g, 'tcx>> for CrateNum {
    fn decode(d: &mut NetListDecoder<'g, 'tcx>) -> Self {
        let stable_id = StableCrateId::decode(d);
        d.generator.tcx.stable_crate_id_to_crate_num(stable_id)
    }
}

impl<'g, 'tcx> Decodable<NetListDecoder<'g, 'tcx>> for DefId {
    fn decode(d: &mut NetListDecoder<'g, 'tcx>) -> Self {
        let def_path_hash = DefPathHash::decode(d);
        d.generator
            .tcx
            .def_path_hash_to_def_id(def_path_hash, &mut || {
                panic!("Failed to convert DefPathHash {def_path_hash:?}")
            })
    }
}

impl<'g, 'tcx> Decodable<NetListDecoder<'g, 'tcx>> for DefIndex {
    fn decode(_d: &mut NetListDecoder<'g, 'tcx>) -> Self {
        panic!("trying to decode `DefIndex` outside the context of a `DefId`")
    }
}
