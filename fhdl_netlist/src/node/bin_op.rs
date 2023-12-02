use std::fmt::{self, Display};

use rustc_macros::{Decodable, Encodable};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NodeOutId, NodeOutIdx, WithId},
    resolver::{Resolve, Resolver},
    sig_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, Encodable, Decodable)]
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

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct BinOpNode {
    bin_op: BinOp,
    inputs: (NodeOutIdx, NodeOutIdx),
    output: NodeOutput,
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
            inputs: (input1.into(), input2.into()),
            output: NodeOutput::wire(ty, sym),
        }
    }

    pub fn bin_op(&self) -> BinOp {
        self.bin_op
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl WithId<ModuleId, &'_ BinOpNode> {
    pub fn left(&self) -> NodeOutId {
        NodeOutId::make(self.id(), self.inputs.0)
    }

    pub fn right(&self) -> NodeOutId {
        NodeOutId::make(self.id(), self.inputs.1)
    }
}

impl<R: Resolver> Resolve<R> for BinOpNode {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(Self {
            bin_op: self.bin_op,
            inputs: self.inputs,
            output: self.output.resolve(resolver)?,
        })
    }
}

impl From<BinOpNode> for NodeKind {
    fn from(node: BinOpNode) -> Self {
        Self::BinOp(node)
    }
}

impl IsNode for BinOpNode {
    type Inputs = (NodeOutIdx, NodeOutIdx);
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
