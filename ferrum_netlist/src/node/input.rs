use std::fmt::Debug;

use ferrum::prim_ty::PrimTy;

use super::{IsNode, Node, NodeOutput};
use crate::{net_kind::NetKind, symbol::Symbol};

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

impl From<InputNode> for Node {
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
