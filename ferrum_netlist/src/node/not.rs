use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_kind::NetKind, net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct NotNode {
    pub input: NodeOutId,
    pub output: NodeOutput,
}

impl NotNode {
    pub fn new(ty: PrimTy, input: NodeOutId, sym: Symbol) -> Self {
        Self {
            input,
            output: NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<NotNode> for NodeKind {
    fn from(node: NotNode) -> Self {
        Self::Not(node)
    }
}

impl IsNode for NotNode {
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
