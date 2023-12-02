use std::{cell::RefCell, rc::Rc};

use fhdl_netlist::{
    group::ItemId,
    net_list::{ModuleId, NodeOutId},
};
use rustc_middle::ty::{EarlyBinder, GenericArgsRef, TyCtxt};
use rustc_type_ir::fold::TypeFoldable;

use crate::generator::GenMode;

#[derive(Clone)]
pub struct EvalContext<'tcx> {
    pub mode: GenMode,
    pub generic_args: GenericArgsRef<'tcx>,
    pub module_id: ModuleId,
    pub closure_inputs: ClosureInputs,
}

impl<'tcx> EvalContext<'tcx> {
    pub fn new(
        mode: GenMode,
        generic_args: GenericArgsRef<'tcx>,
        module_id: ModuleId,
    ) -> Self {
        Self {
            mode,
            generic_args,
            module_id,
            closure_inputs: ClosureInputs::new(),
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
        if self.mode.is_crate() {
            binder.instantiate(tcx, self.generic_args)
        } else {
            binder.instantiate_identity()
        }
    }

    pub fn with_generic_args(&self, generic_args: GenericArgsRef<'tcx>) -> Self {
        Self {
            mode: self.mode,
            generic_args,
            module_id: self.module_id,
            closure_inputs: self.closure_inputs.clone(),
        }
    }

    pub fn new_closure_inputs(&mut self) -> &mut ClosureInputs {
        self.closure_inputs.clear();
        &mut self.closure_inputs
    }

    pub fn next_closure_input(&mut self) -> NodeOutId {
        self.closure_inputs.next_input().expect("no closure inputs")
    }
}

#[derive(Clone)]
pub struct ClosureInputs {
    inputs: Rc<RefCell<Vec<NodeOutId>>>,
    start: usize,
}

impl ClosureInputs {
    fn new() -> Self {
        Self {
            inputs: Rc::new(RefCell::new(Vec::with_capacity(8))),
            start: 0,
        }
    }
    pub fn push(&mut self, item_id: ItemId) -> &mut Self {
        self.inputs.borrow_mut().extend(item_id);
        self
    }

    fn clear(&mut self) {
        self.inputs.borrow_mut().clear();
        self.start = 0;
    }

    fn next_input(&mut self) -> Option<NodeOutId> {
        if self.start < self.inputs.borrow().len() {
            let res = self.inputs.borrow()[self.start];
            self.start += 1;
            Some(res)
        } else {
            None
        }
    }
}
