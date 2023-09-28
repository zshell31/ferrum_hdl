use std::fmt::Debug;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_kind::NetKind, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct InputNode {
    output: NodeOutput,
}

impl InputNode {
    pub fn new(ty: PrimTy, sym: Symbol) -> Self {
        Self {
            output: NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<InputNode> for NodeKind {
    fn from(node: InputNode) -> Self {
        Self::Input(node)
    }
}

impl IsNode for InputNode {
    type Inputs = ();
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &()
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}
