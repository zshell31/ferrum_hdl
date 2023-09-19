use super::{IsNode, Node, NodeOutput};
use crate::{net_kind::NetKind, net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct DFFNode {
    pub inputs: (NodeOutId, NodeOutId, NodeOutId, Option<NodeOutId>),
    pub output: NodeOutput,
    pub rst_val: u128,
}

impl DFFNode {
    pub fn new(
        ty: PrimTy,
        clk: NodeOutId,
        rst: NodeOutId,
        en: Option<NodeOutId>,
        rst_val: u128,
        data: NodeOutId,
        sym: Symbol,
    ) -> Self {
        Self {
            inputs: (clk, data, rst, en),
            output: NodeOutput {
                ty,
                sym,
                kind: NetKind::Reg,
            },
            rst_val,
        }
    }
}

impl From<DFFNode> for Node {
    fn from(node: DFFNode) -> Self {
        Self::DFF(node)
    }
}

impl IsNode for DFFNode {
    type Inputs = (NodeOutId, NodeOutId, NodeOutId, Option<NodeOutId>);
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
