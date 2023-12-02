use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct Not {
    pub input: NodeOutId,
    pub output: NodeOutput,
}

impl Not {
    pub fn new(ty: PrimTy, input: NodeOutId, sym: Symbol) -> Self {
        Self {
            input,
            output: NodeOutput::wire(ty, sym),
        }
    }
}

impl From<Not> for NodeKind {
    fn from(node: Not) -> Self {
        Self::Not(node)
    }
}

impl IsNode for Not {
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
