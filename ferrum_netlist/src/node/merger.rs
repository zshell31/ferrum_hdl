use std::fmt::Debug;

use super::{IsNode, Node, NodeOutput};
use crate::{
    arena::with_arena, net_kind::NetKind, net_list::NodeOutId, sig_ty::PrimTy,
    symbol::Symbol,
};

#[derive(Debug, Clone)]
pub struct Merger {
    pub inputs: &'static [NodeOutId],
    pub output: NodeOutput,
}

impl Merger {
    pub fn new(
        width: u128,
        inputs: impl IntoIterator<Item = NodeOutId>,
        sym: Symbol,
    ) -> Self {
        Self {
            inputs: unsafe { with_arena().alloc_from_iter(inputs) },
            output: NodeOutput {
                ty: PrimTy::BitVec(width),
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<Merger> for Node {
    fn from(node: Merger) -> Self {
        Self::Merger(node)
    }
}

impl IsNode for Merger {
    type Inputs = [NodeOutId];
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        self.inputs
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}
