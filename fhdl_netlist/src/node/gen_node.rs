use rustc_macros::{Decodable, Encodable};

use super::{IsNode, NodeKind, NodeOutput};
use crate::net_list::NodeOutId;

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct GenNode {
    gen_node_idx: u32,
    inputs: Vec<NodeOutId>,
    outputs: Vec<NodeOutput>,
}

impl GenNode {
    pub fn new(
        gen_node_idx: u32,
        inputs: impl IntoIterator<Item = NodeOutId>,
        outputs: impl IntoIterator<Item = NodeOutput>,
    ) -> Self {
        Self {
            gen_node_idx,
            inputs: inputs.into_iter().collect(),
            outputs: outputs.into_iter().collect(),
        }
    }
}

impl From<GenNode> for NodeKind {
    fn from(node: GenNode) -> Self {
        Self::GenNode(node)
    }
}

impl IsNode for GenNode {
    type Inputs = [NodeOutId];
    type Outputs = [NodeOutput];

    fn inputs(&self) -> &Self::Inputs {
        &self.inputs
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut self.inputs
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.outputs
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.outputs
    }
}
