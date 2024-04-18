use fhdl_netlist::netlist::Module;
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::def_id::DefId;
use rustc_middle::{
    mir::{Body, Const as MirConst},
    ty::{EarlyBinder, GenericArgsRef, Ty, TyCtxt},
};
use rustc_span::Span;
use rustc_type_ir::fold::TypeFoldable;

use super::{locals::Locals, Compiler};
use crate::{compiler::item::Item, error::Error};

#[derive(Debug)]
pub struct Context<'tcx> {
    pub generic_args: GenericArgsRef<'tcx>,
    pub module: Module,
    pub locals: Locals<'tcx>,
    pub mir: &'tcx Body<'tcx>,
    pub fn_did: DefId,
    pub in_switch_tuple: bool,
    consts: FxHashMap<MirConst<'tcx>, Item<'tcx>>,
}

impl<'tcx> Context<'tcx> {
    pub fn new(
        fn_did: DefId,
        module: Module,
        generic_args: GenericArgsRef<'tcx>,
        mir: &'tcx Body<'tcx>,
    ) -> Self {
        Self {
            generic_args,
            module,
            locals: Default::default(),
            mir,
            fn_did,
            in_switch_tuple: false,
            consts: Default::default(),
        }
    }

    pub fn instantiate<T: TypeFoldable<TyCtxt<'tcx>>>(
        &self,
        tcx: TyCtxt<'tcx>,
        foldable: T,
    ) -> T {
        let binder = EarlyBinder::bind(foldable);
        binder.instantiate(tcx, self.generic_args)
    }

    #[inline]
    pub fn add_const(&mut self, const_: MirConst<'tcx>, item: Item<'tcx>) {
        self.consts.insert(const_, item);
    }

    #[inline]
    pub fn find_const(&self, const_: &MirConst<'tcx>) -> Option<Item<'tcx>> {
        self.consts.get(const_).cloned()
    }

    #[inline]
    pub fn fn_ty(&self, compiler: &Compiler<'tcx>) -> Ty<'tcx> {
        compiler.type_of(self.fn_did, self.generic_args)
    }

    #[inline]
    pub fn fn_generic_const(
        &self,
        compiler: &mut Compiler<'tcx>,
        idx: usize,
        span: Span,
    ) -> Result<Option<u128>, Error> {
        let fn_ty = self.fn_ty(compiler);
        compiler.generic_const(fn_ty, idx, self.generic_args, span)
    }
}
