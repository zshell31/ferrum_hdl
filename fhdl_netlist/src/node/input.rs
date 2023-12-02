use std::fmt::Debug;

use rustc_macros::{Decodable, Encodable};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, sig_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct Input {
    pub output: NodeOutput,
}

impl Input {
    pub fn new(ty: NodeTy, sym: Option<Symbol>) -> Self {
        Self {
            output: NodeOutput::wire(ty, sym),
        }
    }
}

impl From<Input> for NodeKind {
    fn from(node: Input) -> Self {
        Self::Input(node)
    }
}

impl IsNode for Input {
    type Inputs = [NodeOutId];
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
