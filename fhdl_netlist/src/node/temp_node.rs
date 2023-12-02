use rustc_macros::{Decodable, Encodable};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{NodeOutId, NodeOutIdx, TempNodeId},
    resolver::{Resolve, Resolver},
};

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct TemplateNode {
    id: TempNodeId,
    inputs: Vec<NodeOutIdx>,
    outputs: Vec<NodeOutput>,
}

impl TemplateNode {
    pub fn new(
        id: TempNodeId,
        inputs: impl IntoIterator<Item = NodeOutId>,
        outputs: impl IntoIterator<Item = NodeOutput>,
    ) -> Self {
        Self {
            id,
            inputs: inputs.into_iter().map(Into::into).collect(),
            outputs: outputs.into_iter().collect(),
        }
    }

    pub fn temp_node_id(&self) -> TempNodeId {
        self.id
    }
}

impl<R: Resolver> Resolve<R> for TemplateNode {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(Self {
            id: self.id,
            inputs: self.inputs.clone(),
            outputs: self.outputs.resolve(resolver)?,
        })
    }
}

impl From<TemplateNode> for NodeKind {
    fn from(node: TemplateNode) -> Self {
        Self::TemplateNode(node)
    }
}

impl IsNode for TemplateNode {
    type Inputs = [NodeOutIdx];
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
