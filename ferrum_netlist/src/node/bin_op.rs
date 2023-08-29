use std::fmt::{self, Display};

use ferrum::prim_ty::PrimTy;

use super::{IsNode, Node, NodeOutput};
use crate::{net_kind::NetKind, net_list::NodeOutId, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    BitAnd,
    BitOr,
    BitXor,
    And,
    Or,
    Add,
    Sub,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::And => "&&",
            Self::Or => "||",
            Self::Add => "+",
            Self::Sub => "-",
        })
    }
}

#[derive(Debug, Clone)]
pub struct BinOpNode {
    pub bin_op: BinOp,
    pub inputs: (NodeOutId, NodeOutId),
    pub output: NodeOutput,
}

impl BinOpNode {
    pub fn new(
        ty: PrimTy,
        bin_op: BinOp,
        input1: NodeOutId,
        input2: NodeOutId,
        sym: Symbol,
    ) -> Self {
        Self {
            bin_op,
            inputs: (input1, input2),
            output: NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            },
        }
    }
}

impl From<BinOpNode> for Node {
    fn from(node: BinOpNode) -> Self {
        Self::BinOp(node)
    }
}

impl IsNode for BinOpNode {
    type Inputs = (NodeOutId, NodeOutId);
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &self.inputs
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}
