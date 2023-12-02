use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct BitNotNode {
    pub input: NodeOutId,
    pub output: NodeOutput,
}

impl BitNotNode {
    pub fn new(ty: PrimTy, input: NodeOutId, sym: Symbol) -> Self {
        Self {
            input,
            output: NodeOutput::wire(ty, sym),
        }
    }
}

impl From<BitNotNode> for NodeKind {
    fn from(node: BitNotNode) -> Self {
        Self::BitNot(node)
    }
}

impl IsNode for BitNotNode {
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