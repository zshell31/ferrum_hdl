use super::{IsNode, NodeKind, NodeOutput};
use crate::{arena::with_arena, net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug)]
pub struct Pass {
    pub input: NodeOutId,
    pub output: NodeOutput,
}

impl Pass {
    pub fn new(ty: PrimTy, input: NodeOutId, sym: Symbol) -> Self {
        Self {
            input,
            output: NodeOutput::wire(ty, sym),
        }
    }
}

impl From<Pass> for NodeKind {
    fn from(node: Pass) -> Self {
        Self::Pass(node)
    }
}

impl IsNode for Pass {
    type Inputs = NodeOutId;
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &self.input
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}

#[derive(Debug)]
pub struct MultiPass {
    pub inputs: &'static [NodeOutId],
    pub outputs: &'static mut [NodeOutput],
}

impl MultiPass {
    pub fn new(
        inputs: impl IntoIterator<Item = NodeOutId>,
        outputs: impl IntoIterator<Item = (PrimTy, Symbol)>,
    ) -> Self {
        let node = Self {
            inputs: unsafe { with_arena().alloc_from_iter(inputs) },
            outputs: unsafe {
                with_arena().alloc_from_iter(
                    outputs
                        .into_iter()
                        .map(|(ty, sym)| NodeOutput::wire(ty, sym)),
                )
            },
        };
        assert_eq!(node.inputs.len(), node.outputs.len());

        node
    }
}

impl From<MultiPass> for NodeKind {
    fn from(node: MultiPass) -> Self {
        Self::MultiPass(node)
    }
}

impl IsNode for MultiPass {
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
