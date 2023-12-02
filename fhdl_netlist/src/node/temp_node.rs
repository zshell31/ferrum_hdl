use std::iter;

use rustc_macros::{Decodable, Encodable};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{InOut, NodeOutIdx, TempNodeId},
    resolver::{Resolve, Resolver},
    sig_ty::NodeTy,
};

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct TemplateNode {
    id: TempNodeId,
    outputs: VirtualOutputs,
}

impl TemplateNode {
    pub fn new(id: TempNodeId, output_ty: NodeTy) -> Self {
        Self {
            id,
            outputs: VirtualOutputs(NodeOutput::wire(output_ty, None)),
        }
    }

    pub fn temp_node_id(&self) -> TempNodeId {
        self.id
    }
}

impl<R: Resolver> Resolve<R> for TemplateNode {
    fn resolve(&self, _resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(Self {
            id: self.id,
            outputs: self.outputs,
        })
    }
}

impl From<TemplateNode> for NodeKind {
    fn from(node: TemplateNode) -> Self {
        Self::TemplateNode(node)
    }
}

#[derive(Debug, Clone, Copy, Encodable, Decodable)]
pub struct VirtualOutputs(NodeOutput);

impl InOut<NodeOutput> for VirtualOutputs {
    fn items_len(&self) -> usize {
        0
    }

    fn items(&self) -> impl Iterator<Item = (usize, &NodeOutput)> {
        iter::empty()
    }

    fn items_mut(&mut self) -> impl Iterator<Item = (usize, &mut NodeOutput)> {
        iter::empty()
    }

    fn by_ind(&self, _ind: usize) -> &NodeOutput {
        &self.0
    }

    fn by_ind_mut(&mut self, _ind: usize) -> &mut NodeOutput {
        &mut self.0
    }
}

impl IsNode for TemplateNode {
    type Inputs = [NodeOutIdx];
    type Outputs = VirtualOutputs;

    fn inputs(&self) -> &Self::Inputs {
        &[]
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut []
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.outputs
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.outputs
    }
}
