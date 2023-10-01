use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    arena::with_arena,
    net_list::{ModuleId, NodeOutId},
    sig_ty::PrimTy,
    symbol::Symbol,
};

#[derive(Debug)]
pub struct ModInst {
    pub name: Symbol,
    pub module_id: ModuleId,
    pub inputs: &'static [NodeOutId],
    pub outputs: &'static mut [NodeOutput],
}

impl ModInst {
    pub fn new(
        name: Symbol,
        module_id: ModuleId,
        inputs: impl IntoIterator<Item = NodeOutId>,
        outputs: impl IntoIterator<Item = (PrimTy, Symbol)>,
    ) -> Self {
        Self {
            name,
            module_id,
            inputs: unsafe { with_arena().alloc_from_iter(inputs) },
            outputs: unsafe {
                with_arena().alloc_from_iter(
                    outputs
                        .into_iter()
                        .map(|(ty, sym)| NodeOutput::wire(ty, sym)),
                )
            },
        }
    }
}

impl From<ModInst> for NodeKind {
    fn from(node: ModInst) -> Self {
        Self::ModInst(node)
    }
}

impl IsNode for ModInst {
    type Inputs = [NodeOutId];
    type Outputs = [NodeOutput];

    fn inputs(&self) -> &Self::Inputs {
        self.inputs
    }

    fn outputs(&self) -> &Self::Outputs {
        self.outputs
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        self.outputs
    }
}
