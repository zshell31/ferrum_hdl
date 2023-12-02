use std::fmt::Debug;

use smallvec::SmallVec;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, sig_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct Merger {
    pub inputs: SmallVec<[NodeOutId; 8]>,
    pub output: NodeOutput,
    pub rev: bool,
}

impl Merger {
    pub fn new(
        width: u128,
        inputs: impl IntoIterator<Item = NodeOutId>,
        rev: bool,
        sym: impl Into<Option<Symbol>>,
    ) -> Self {
        Self {
            inputs: inputs.into_iter().collect(),
            output: NodeOutput::wire(NodeTy::BitVec(width), sym.into()),
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
