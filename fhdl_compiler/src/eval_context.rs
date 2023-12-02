use fhdl_netlist::{
    group::ItemId,
    net_list::{ModuleId, NodeOutId},
};
use rustc_middle::ty::{EarlyBinder, GenericArgsRef, TyCtxt};
use rustc_type_ir::fold::TypeFoldable;
use smallvec::SmallVec;

#[derive(Clone)]
pub struct EvalContext<'tcx> {
    pub generic_args: GenericArgsRef<'tcx>,
    pub module_id: ModuleId,
    pub closure_inputs: ClosureInputs,
}

impl<'tcx> EvalContext<'tcx> {
    pub fn new(generic_args: GenericArgsRef<'tcx>, module_id: ModuleId) -> Self {
        Self {
            generic_args,
            module_id,
            closure_inputs: Default::default(),
        }
    }

    pub fn instantiate<T: TypeFoldable<TyCtxt<'tcx>>>(
        &self,
        tcx: TyCtxt<'tcx>,
        foldable: T,
    ) -> T {
        EarlyBinder::bind(foldable).instantiate(tcx, self.generic_args)
    }

    pub fn new_closure_inputs(&mut self) -> &mut ClosureInputs {
        self.closure_inputs.clear();
        &mut self.closure_inputs
    }

    pub fn next_closure_input(&mut self) -> NodeOutId {
        self.closure_inputs.next_input().expect("no closure inputs")
    }
}

#[derive(Default, Clone)]
pub struct ClosureInputs {
    inputs: SmallVec<[NodeOutId; 8]>,
    start: usize,
}

impl ClosureInputs {
    pub fn push(&mut self, item_id: ItemId) -> &mut Self {
        self.inputs.extend(item_id);
        self
    }

    fn clear(&mut self) {
        self.inputs.clear();
        self.start = 0;
    }

    fn next_input(&mut self) -> Option<NodeOutId> {
        if self.start < self.inputs.len() {
            let res = self.inputs[self.start];
            self.start += 1;
            Some(res)
        } else {
            None
        }
    }
}
