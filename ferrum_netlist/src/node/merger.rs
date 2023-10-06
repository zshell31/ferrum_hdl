use std::fmt::Debug;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{arena::Vec, net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct Merger {
    pub inputs: Vec<NodeOutId>,
    pub output: NodeOutput,
    pub rev: bool,
}

impl Merger {
    pub fn new(
        width: u128,
        inputs: impl IntoIterator<Item = NodeOutId>,
        sym: Symbol,
        rev: bool,
    ) -> Self {
        Self {
            inputs: Vec::collect_from(inputs),
            output: NodeOutput::wire(PrimTy::BitVec(width), sym),
            rev,
        }
    }
}

impl From<Merger> for NodeKind {
    fn from(node: Merger) -> Self {
        Self::Merger(node)
    }
}

impl IsNode for Merger {
    type Inputs = [NodeOutId];
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        self.inputs.as_slice()
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        self.inputs.as_mut_slice()
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}
