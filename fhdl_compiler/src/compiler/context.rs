use fhdl_netlist::netlist::Module;
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::def_id::DefId;
use rustc_middle::{
    mir::{Body, Const as MirConst, Local},
    ty::{EarlyBinder, GenericArgsRef, TyCtxt},
};
use rustc_type_ir::fold::TypeFoldable;

use super::switch::Switch;
use crate::compiler::item::Item;

#[derive(Debug, Default, Clone)]
pub struct Locals<'tcx>(FxHashMap<Local, Item<'tcx>>);

impl<'tcx> Locals<'tcx> {
    pub fn place(&mut self, local: Local, item: Item<'tcx>) -> Local {
        self.0.insert(local, item);
        local
    }

    pub fn get_opt(&self, local: Local) -> Option<Item<'tcx>> {
        self.0.get(&local).cloned()
    }

    pub fn get(&self, local: Local) -> Item<'tcx> {
        match self.0.get(&local) {
            Some(item) => item.clone(),
            None => panic!("cannot find item for local {local:?}"),
        }
    }

    pub fn get_mut(&mut self, local: Local) -> &mut Item<'tcx> {
        match self.0.get_mut(&local) {
            Some(item) => item,
            None => panic!("cannot find item for local {local:?}"),
        }
    }
}

#[derive(Debug)]
pub struct Context<'tcx> {
    pub generic_args: GenericArgsRef<'tcx>,
    pub module: Module,
    pub locals: Locals<'tcx>,
    pub mir: &'tcx Body<'tcx>,
    pub fn_did: DefId,
    consts: FxHashMap<MirConst<'tcx>, Item<'tcx>>,
    switches: Vec<Switch<'tcx>>,
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
            consts: Default::default(),
            switches: Default::default(),
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

    pub fn add_const(&mut self, const_: MirConst<'tcx>, item: Item<'tcx>) {
        self.consts.insert(const_, item);
    }

    pub fn find_const(&self, const_: &MirConst<'tcx>) -> Option<Item<'tcx>> {
        self.consts.get(const_).cloned()
    }

    pub fn push_switch(&mut self, switch: Switch<'tcx>) {
        self.switches.push(switch)
    }

    pub fn pop_switch(&mut self) -> Option<Switch<'tcx>> {
        self.switches.pop()
    }

    pub fn last_switch(&mut self) -> Option<&mut Switch<'tcx>> {
        self.switches.last_mut()
    }
}
