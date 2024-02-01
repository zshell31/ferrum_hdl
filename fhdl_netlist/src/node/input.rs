use std::fmt::Debug;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutIdx, node_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct Input {
    output: NodeOutput,
}

impl Input {
    pub fn new(ty: NodeTy, sym: Option<Symbol>) -> Self {
        Self {
            output: NodeOutput::wire(ty, sym),
        }
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl From<Input> for NodeKind {
    fn from(node: Input) -> Self {
        Self::Input(node)
    }
}

impl IsNode for Input {
    type Inputs = [NodeOutIdx];
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &[]
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut []
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}
