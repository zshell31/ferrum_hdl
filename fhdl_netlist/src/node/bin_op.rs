use std::fmt::{self, Display};

use rustc_macros::{Decodable, Encodable};

use super::{assert_width, IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NetList, NodeOutId, NodeOutIdx, WithId},
    resolver::{Resolve, Resolver},
    sig_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, Encodable, Decodable)]
pub enum BinOp {
    BitAnd,
    BitOr,
    BitXor,
    Add,
    Sub,
    Div,
    Mul,
    Rem,
    Shl,
    Shr,
    Eq,
    Ne,
    Ge,
    Gt,
    Le,
    Lt,
    And,
    Or,
}

impl BinOp {
    pub fn should_convert_operands(&self) -> bool {
        use BinOp::*;

        matches!(
            self,
            BitAnd | BitOr | BitXor | Add | Sub | Mul | Div | Rem | Shl | Shr
        )
    }
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

    fn assert(&self, module_id: ModuleId, net_list: &NetList) {
        let node = WithId::<ModuleId, _>::new(module_id, self);
        let lhs = &net_list[node.left()];
        let rhs = &net_list[node.right()];

        match self.bin_op() {
            BinOp::Add
            | BinOp::And
            | BinOp::BitAnd
            | BinOp::BitOr
            | BinOp::BitXor
            | BinOp::Sub
            | BinOp::Div
            | BinOp::Mul
            | BinOp::Or
            | BinOp::Rem
            | BinOp::Shl
            | BinOp::Shr => {
                assert_width!(self.output.width(), lhs.width());
                assert_width!(self.output.width(), rhs.width());
            }
            BinOp::Eq | BinOp::Ge | BinOp::Gt | BinOp::Le | BinOp::Lt | BinOp::Ne => {
                assert_width!(lhs.width(), rhs.width());
            }
        }
    }
}
