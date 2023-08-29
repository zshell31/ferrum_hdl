use ferrum::prim_ty::PrimTy;

use super::{IsNode, Node, NodeOutput};
use crate::{net_kind::NetKind, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct ConstNode {
    pub value: u128,
    pub inject: bool,
    pub output: NodeOutput,
}

impl ConstNode {
    pub fn new(ty: PrimTy, value: u128, sym: Symbol) -> Self {
        Self {
            value,
            inject: false,
            output: NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<ConstNode> for Node {
    fn from(node: ConstNode) -> Self {
        Self::Const(node)
    }
}

impl IsNode for ConstNode {
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
