use std::{io, path::Path};

use rustc_const_eval::interpret::{AllocDecodingSession, AllocId};
use rustc_data_structures::fx::{FxHashMap, FxIndexSet};
use rustc_hir::def_id::{CrateNum, DefId};
use rustc_middle::ty::{PredicateKind, Ty, TyCtxt, TyEncoder};
use rustc_serialize::{
    opaque::{FileEncoder, MemDecoder},
    Encodable, Encoder,
};
use rustc_type_ir::TyDecoder;

pub struct CacheEncoder<'tcx> {
    tcx: TyCtxt<'tcx>,
    encoder: FileEncoder,
    type_shorthands: FxHashMap<Ty<'tcx>, usize>,
    predicate_shorthands: FxHashMap<PredicateKind<'tcx>, usize>,
    interpret_allocs: FxIndexSet<AllocId>,
}

impl<'tcx> CacheEncoder<'tcx> {
    pub fn new<P: AsRef<Path>>(tcx: TyCtxt<'tcx>, path: P) -> io::Result<Self> {
        Ok(Self {
            tcx,
            encoder: FileEncoder::new(path)?,
            type_shorthands: Default::default(),
            predicate_shorthands: Default::default(),
            interpret_allocs: Default::default(),
        })
    }

    pub fn dump(&mut self, ty: Ty<'tcx>) {
        ty.encode(self)
    }
}

macro_rules! encoder_methods {
    ($($name:ident($ty:ty);)*) => {
        #[inline]
        $(fn $name(&mut self, value: $ty) {
            self.encoder.$name(value)
        })*
    }
}

impl<'tcx> Encoder for CacheEncoder<'tcx> {
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

impl<'tcx> TyEncoder for CacheEncoder<'tcx> {
    type I = TyCtxt<'tcx>;
    const CLEAR_CROSS_CRATE: bool = false;

    fn position(&self) -> usize {
        self.encoder.position()
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

impl<'tcx> Encodable<CacheEncoder<'tcx>> for CrateNum {
    #[inline]
    fn encode(&self, e: &mut CacheEncoder<'tcx>) {
        todo!()
    }
}

impl<'tcx> Encodable<CacheEncoder<'tcx>> for DefId {
    #[inline]
    fn encode(&self, e: &mut CacheEncoder<'tcx>) {
        todo!()
    }
}

pub struct CacheDecoder<'a, 'tcx> {
    tcx: TyCtxt<'tcx>,
    opaque: MemDecoder<'a>,
    alloc_decoding_session: AllocDecodingSession<'a>,
}

rustc_middle::implement_ty_decoder!(CacheDecoder<'a, 'tcx>);

// impl<'a, 'tcx> TyDecoder for CacheDecoder<'a, 'tcx> {
//     type I = TyCtxt<'tcx>;
//     const CLEAR_CROSS_CRATE: bool = false;

//     fn interner(&self) -> Self::I {
//         self.tcx
//     }

//     fn cached_ty_for_shorthand<F>(
//         &mut self,
//         shorthand: usize,
//         or_insert_with: F,
//     ) -> <Self::I as rustc_type_ir::Interner>::Ty
//     where
//         F: FnOnce(&mut Self) -> <Self::I as rustc_type_ir::Interner>::Ty,
//     {
//         :
//     }

//     fn with_position<F, R>(&mut self, pos: usize, f: F) -> R
//     where
//         F: FnOnce(&mut Self) -> R,
//     {
//         todo!()
//     }

//     fn decode_alloc_id(&mut self) -> <Self::I as rustc_type_ir::Interner>::AllocId {
//         todo!()
//     }
// }
