use std::fmt::{self, Display};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, sig_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    And,
    BitAnd,
    BitOr,
    BitXor,
    Div,
    Eq,
    Ge,
    Gt,
    Le,
    Lt,
    Mul,
    Ne,
    Or,
    Rem,
    Shl,
    Shr,
    Sub,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Add => "+",
            Self::And => "&&",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::Div => "/",
            Self::Eq => "==",
            Self::Ge => ">=",
            Self::Gt => ">",
            Self::Le => "<=",
            Self::Lt => "<",
            Self::Mul => "*",
            Self::Ne => "!=",
            Self::Or => "||",
            Self::Rem => "%",
            Self::Shl => "<<",
            Self::Shr => ">>",
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
        ty: NodeTy,
        bin_op: BinOp,
        input1: NodeOutId,
        input2: NodeOutId,
        sym: Option<Symbol>,
    ) -> Self {
        Self {
            bin_op,
            inputs: (input1, input2),
            output: NodeOutput::wire(ty, sym),
        }
    }
}

impl From<BinOpNode> for NodeKind {
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

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut self.inputs
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}
