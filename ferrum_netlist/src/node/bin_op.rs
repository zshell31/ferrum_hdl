use std::fmt::{self, Display};

use ferrum::prim_ty::{DummyTy, PrimTy};

use super::{IsNode, Node};
use crate::{net_kind::NetKind, net_list::NodeId, output::NodeOutput, symbol::Symbol};

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

#[derive(Debug, Clone, Copy)]
pub struct BinOpNode {
    pub bin_op: BinOp,
    pub input1: NodeId,
    pub input2: NodeId,
    pub out: NodeOutput,
}

impl BinOpNode {
    pub fn new(
        ty: PrimTy,
        bin_op: BinOp,
        input1: NodeId,
        input2: NodeId,
        sym: Symbol,
    ) -> Self {
        Self {
            bin_op,
            input1,
            input2,
            out: NodeOutput {
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
    type Outputs = (DummyTy,);

    fn node_output(&self, out: u8) -> &NodeOutput {
        match out {
            0 => &self.out,
            _ => unreachable!(),
        }
    }

    fn node_output_mut(&mut self, out: u8) -> &mut NodeOutput {
        match out {
            0 => &mut self.out,
            _ => unreachable!(),
        }
    }

    fn inputs(&self) -> impl Iterator<Item = NodeId> {
        [self.input1, self.input2].into_iter()
    }
}
