use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct ZeroExtend {
    pub input: NodeOutId,
    pub output: NodeOutput,
}

impl ZeroExtend {
    pub fn new(ty: PrimTy, input: NodeOutId, sym: impl Into<Option<Symbol>>) -> Self {
        Self {
            input,
            output: NodeOutput::wire(ty, sym.into()),
        }
    }
}

impl From<ZeroExtend> for NodeKind {
    fn from(node: ZeroExtend) -> Self {
        Self::ZeroExtend(node)
    }
}

impl IsNode for ZeroExtend {
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
