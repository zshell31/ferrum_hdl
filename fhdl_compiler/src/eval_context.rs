use std::{cell::RefCell, rc::Rc};

use fhdl_netlist::{
    group::ItemId,
    net_list::{ModuleId, NodeOutId},
};
use rustc_middle::ty::{EarlyBinder, GenericArgsRef, TyCtxt};
use rustc_type_ir::fold::TypeFoldable;

#[derive(Clone, Debug)]
pub struct EvalContext<'tcx> {
    pub is_primary: bool,
    pub generic_args: GenericArgsRef<'tcx>,
    pub module_id: ModuleId,
    closure_inputs: ClosureInputs,
    closure_as_module: bool,
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
            closure_inputs: ClosureInputs::new(),
            closure_as_module: false,
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
            closure_inputs: self.closure_inputs.clone(),
            closure_as_module: self.closure_as_module,
        }
    }

    pub fn new_closure_inputs(&mut self) -> &mut ClosureInputs {
        self.closure_inputs.clear();
        &mut self.closure_inputs
    }

    pub fn next_closure_input(&mut self) -> NodeOutId {
        self.closure_inputs.next_input().expect("no closure inputs")
    }

    pub fn eval_closure_as_module<T>(
        &mut self,
        module_id: ModuleId,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> T {
        let old_module_id = self.module_id;

        self.module_id = module_id;
        self.closure_as_module = true;

        let res = f(self);

        self.closure_as_module = false;
        self.module_id = old_module_id;

        res
    }

    pub fn closure_as_module(&self) -> bool {
        self.closure_as_module
    }
}

#[derive(Clone, Debug)]
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
