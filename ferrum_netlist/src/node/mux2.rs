use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_kind::NetKind, net_list::NodeOutId, params::Inputs, sig_ty::PrimTy,
    symbol::Symbol,
};

#[derive(Debug, Clone)]
pub struct Mux2Node {
    pub inputs: (NodeOutId, (NodeOutId, NodeOutId)),
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
            inputs: (sel, (input1, input2)),
            output: NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<Mux2Node> for NodeKind {
    fn from(node: Mux2Node) -> Self {
        Self::Mux2(node)
    }
}

impl IsNode for Mux2Node {
    type Inputs = (NodeOutId, (NodeOutId, NodeOutId));
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

impl Inputs for (NodeOutId, (NodeOutId, NodeOutId)) {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        [self.0, self.1 .0, self.1 .1].into_iter()
    }

    fn len(&self) -> usize {
        3
    }
}
