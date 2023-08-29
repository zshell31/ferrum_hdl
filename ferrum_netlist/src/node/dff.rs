use ferrum::prim_ty::PrimTy;

use super::{IsNode, Node, NodeOutput};
use crate::{net_kind::NetKind, net_list::NodeOutId, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct DFFNode {
    pub inputs: (NodeOutId, NodeOutId, NodeOutId),
    pub output: NodeOutput,
}

impl DFFNode {
    pub fn new(
        ty: PrimTy,
        clk: NodeOutId,
        rst_val: NodeOutId,
        data: NodeOutId,
        sym: Symbol,
    ) -> Self {
        Self {
            inputs: (clk, rst_val, data),
            output: NodeOutput {
                ty,
                sym,
                kind: NetKind::Reg,
            },
        }
    }
}

impl From<DFFNode> for Node {
    fn from(node: DFFNode) -> Self {
        Self::DFF(node)
    }
}

impl IsNode for DFFNode {
    type Inputs = (NodeOutId, NodeOutId, NodeOutId);
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
