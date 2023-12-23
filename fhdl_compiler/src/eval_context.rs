use fhdl_netlist::{group::ItemId, net_list::ModuleId, sig_ty::SignalTy};
use rustc_data_structures::fx::FxHashMap;
use rustc_index::IndexVec;
use rustc_middle::{
    mir::{Const as MirConst, Local, Operand},
    ty::{EarlyBinder, GenericArgsRef, TyCtxt},
};
use rustc_span::Span;
use rustc_target::abi::FieldIdx;
use rustc_type_ir::fold::TypeFoldable;

use crate::error::{Error, SpanError, SpanErrorKind};

#[derive(Debug, Clone, Copy)]
pub enum ModuleOrItem {
    Module(ModuleId),
    Item(ItemId),
}

impl ModuleOrItem {
    pub fn mod_id(self) -> ModuleId {
        match self {
            Self::Module(mod_id) => mod_id,
            _ => panic!("expected module id, not item id"),
        }
    }

    pub fn item_id(self) -> ItemId {
        match self {
            Self::Item(item_id) => item_id,
            _ => panic!("exptected item id, not module id"),
        }
    }
}

impl From<ModuleId> for ModuleOrItem {
    fn from(mod_id: ModuleId) -> Self {
        Self::Module(mod_id)
    }
}

impl From<ItemId> for ModuleOrItem {
    fn from(item_id: ItemId) -> Self {
        Self::Item(item_id)
    }
}

#[derive(Debug, Clone)]
pub struct Closure<'tcx> {
    pub upvars: IndexVec<FieldIdx, Operand<'tcx>>,
    pub output_ty: SignalTy,
}

#[derive(Debug, Clone)]
pub struct EvalContext<'tcx> {
    pub is_primary: bool,
    pub generic_args: GenericArgsRef<'tcx>,
    pub module_id: ModuleId,
    pub locals: FxHashMap<Local, ModuleOrItem>,
    pub consts: FxHashMap<MirConst<'tcx>, ItemId>,
    pub closures: FxHashMap<ModuleId, Closure<'tcx>>,
}

impl<'tcx> EvalContext<'tcx> {
    pub fn new(
        is_primary: bool,
        generic_args: GenericArgsRef<'tcx>,
        module_id: ModuleId,
    ) -> Self {
        Self {
            is_primary,
            generic_args,
            module_id,
            locals: Default::default(),
            consts: Default::default(),
            closures: Default::default(),
        }
    }

    pub fn instantiate<T: TypeFoldable<TyCtxt<'tcx>>>(
        &self,
        tcx: TyCtxt<'tcx>,
        foldable: T,
    ) -> T {
        let binder = EarlyBinder::bind(foldable);
        self.instantiate_early_binder(tcx, binder)
    }

    pub fn instantiate_early_binder<T: TypeFoldable<TyCtxt<'tcx>>>(
        &self,
        tcx: TyCtxt<'tcx>,
        binder: EarlyBinder<T>,
    ) -> T {
        if self.is_primary {
            binder.instantiate(tcx, self.generic_args)
        } else {
            binder.instantiate_identity()
        }
    }

    pub fn with_generic_args(&self, generic_args: GenericArgsRef<'tcx>) -> Self {
        Self {
            is_primary: self.is_primary,
            generic_args,
            module_id: self.module_id,
            locals: Default::default(),
            consts: Default::default(),
            closures: Default::default(),
        }
    }

    pub fn with_module_id(&self, module_id: ModuleId) -> Self {
        Self {
            is_primary: self.is_primary,
            generic_args: self.generic_args,
            module_id,
            locals: Default::default(),
            consts: Default::default(),
            closures: Default::default(),
        }
    }

    pub fn add_local(&mut self, local: Local, mod_or_item: impl Into<ModuleOrItem>) {
        self.locals.insert(local, mod_or_item.into());
    }

    pub fn find_local(&self, local: Local, span: Span) -> Result<ModuleOrItem, Error> {
        self.locals.get(&local).copied().ok_or_else(|| {
            SpanError::new(SpanErrorKind::MissingLocal(local), span).into()
        })
    }

    pub fn add_const(&mut self, const_: MirConst<'tcx>, item_id: ItemId) {
        self.consts.insert(const_, item_id);
    }

    pub fn find_const(&self, const_: &MirConst<'tcx>) -> Option<ItemId> {
        self.consts.get(const_).copied()
    }

    pub fn add_closure(&mut self, module_id: ModuleId, closure: Closure<'tcx>) {
        self.closures.insert(module_id, closure);
    }

    pub fn find_closure(
        &self,
        module_id: ModuleId,
        span: Span,
    ) -> Result<&Closure<'tcx>, Error> {
        self.closures
            .get(&module_id)
            .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedClosure, span).into())
    }
}
