use rustc_macros::{Decodable, Encodable};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NodeOutId, NodeOutIdx, WithId},
    sig_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, Encodable, Decodable)]
pub struct Not {
    input: NodeOutIdx,
    output: NodeOutput,
}

impl Not {
    pub fn new(ty: NodeTy, input: NodeOutId, sym: Option<Symbol>) -> Self {
        Self {
            input: input.into(),
            output: NodeOutput::wire(ty, sym),
        }
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl WithId<ModuleId, &'_ Not> {
    pub fn input(&self) -> NodeOutId {
        NodeOutId::make(self.id(), self.input)
    }
}

impl From<Not> for NodeKind {
    fn from(node: Not) -> Self {
        Self::Not(node)
    }
}

impl IsNode for Not {
    type Inputs = NodeOutIdx;
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
