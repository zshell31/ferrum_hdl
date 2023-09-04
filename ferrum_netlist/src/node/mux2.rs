use super::{IsNode, Node, NodeOutput};
use crate::{net_kind::NetKind, net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct Mux2Node {
    pub sel: NodeOutId,
    pub inputs: (NodeOutId, NodeOutId),
    pub output: NodeOutput,
}

impl Mux2Node {
    pub fn new(
        ty: PrimTy,
        sel: NodeOutId,
        input1: NodeOutId,
        input2: NodeOutId,
        sym: Symbol,
    ) -> Self {
        Self {
            sel,
            inputs: (input1, input2),
            output: NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<Mux2Node> for Node {
    fn from(node: Mux2Node) -> Self {
        Self::Mux2(node)
    }
}

impl IsNode for Mux2Node {
    type Inputs = (NodeOutId, NodeOutId);
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &self.inputs
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}
