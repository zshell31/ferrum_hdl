use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct BitNot {
    pub input: NodeOutId,
    pub output: NodeOutput,
}

impl BitNot {
    pub fn new(ty: PrimTy, input: NodeOutId, sym: Option<Symbol>) -> Self {
        Self {
            input,
            output: NodeOutput::wire(ty, sym),
        }
    }
}

impl From<BitNot> for NodeKind {
    fn from(node: BitNot) -> Self {
        Self::BitNot(node)
    }
}

impl IsNode for BitNot {
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
