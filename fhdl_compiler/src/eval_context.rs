use fhdl_netlist::{
    group::ItemId,
    net_list::{ModuleId, NodeOutId},
};
use rustc_middle::ty::{EarlyBinder, GenericArgsRef, TyCtxt};
use rustc_span::Span;
use rustc_type_ir::fold::TypeFoldable;
use smallvec::SmallVec;

use crate::error::{Error, SpanError, SpanErrorKind};

#[derive(Clone)]
pub struct EvalContext<'tcx> {
    pub generic_args: GenericArgsRef<'tcx>,
    pub module_id: ModuleId,
    closure_inputs: ClosureInputs,
    fsm: Option<Fsm>,
}

impl<'tcx> EvalContext<'tcx> {
    pub fn new(generic_args: GenericArgsRef<'tcx>, module_id: ModuleId) -> Self {
        Self {
            generic_args,
            module_id,
            closure_inputs: Default::default(),
            fsm: None,
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

    pub fn new_fsm(&mut self) {
        self.fsm.replace(Default::default());
    }

    pub fn is_fsm(&self) -> bool {
        self.fsm.is_some()
    }

    pub fn set_fsm_has_loop(&mut self) {
        if let Some(fsm) = self.fsm.as_mut() {
            fsm.has_loop = true;
        }
    }

    pub fn add_fsm_state(&mut self, state: ItemId) {
        if let Some(fsm) = self.fsm.as_mut() {
            fsm.add_state(state)
        }
    }

    pub fn take_fsm_states(
        &mut self,
        span: Span,
    ) -> Option<Result<SmallVec<[ItemId; 8]>, Error>> {
        let fsm = self.fsm.take()?;

        if !fsm.has_loop {
            return Some(Err(
                SpanError::new(SpanErrorKind::ExpectedLoopInFsm, span).into()
            ));
        }

        Some(Ok(fsm.states))
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

#[derive(Default, Clone)]
pub struct Fsm {
    states: SmallVec<[ItemId; 8]>,
    has_loop: bool,
}

impl Fsm {
    fn add_state(&mut self, state: ItemId) {
        self.states.push(state);
    }
}
