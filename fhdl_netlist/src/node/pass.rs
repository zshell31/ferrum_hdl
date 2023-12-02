use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, sig_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct Pass {
    pub input: NodeOutId,
    pub output: NodeOutput,
}

impl Pass {
    pub fn new(ty: NodeTy, input: NodeOutId, sym: impl Into<Option<Symbol>>) -> Self {
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
