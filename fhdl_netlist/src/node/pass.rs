use super::{IsNode, NodeKind, NodeOutput};
use crate::{arena::Vec, net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct Pass {
    pub input: NodeOutId,
    pub output: NodeOutput,
}

impl Pass {
    pub fn new(ty: PrimTy, input: NodeOutId, sym: impl Into<Option<Symbol>>) -> Self {
        Self {
            input,
            output: NodeOutput::wire(ty, sym.into()),
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

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut self.input
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}

#[derive(Debug, Clone)]
pub struct MultiPass {
    pub inputs: Vec<NodeOutId>,
    pub outputs: Vec<NodeOutput>,
}

impl MultiPass {
    pub fn new(
        inputs: impl IntoIterator<Item = NodeOutId>,
        outputs: impl IntoIterator<Item = (PrimTy, Option<Symbol>)>,
    ) -> Self {
        let node = Self {
            inputs: Vec::collect_from(inputs),
            outputs: Vec::collect_from(
                outputs
                    .into_iter()
                    .map(|(ty, sym)| NodeOutput::wire(ty, sym)),
            ),
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
        self.inputs.as_slice()
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        self.inputs.as_mut_slice()
    }

    fn outputs(&self) -> &Self::Outputs {
        self.outputs.as_slice()
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        self.outputs.as_mut_slice()
    }
}
