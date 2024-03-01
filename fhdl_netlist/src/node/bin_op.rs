use std::fmt::{self, Display};

use super::{IsNode, MakeNode, NodeOutput};
use crate::{
    netlist::{Cursor, Module, NodeId, Port, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub struct BinOpNode {
    pub bin_op: BinOp,
    pub output: [NodeOutput; 1],
}

#[derive(Debug)]
pub struct BinOpArgs {
    pub ty: NodeTy,
    pub bin_op: BinOp,
    pub lhs: Port,
    pub rhs: Port,
    pub sym: Option<Symbol>,
}

impl BinOpArgs {
    fn assert(&self, module: &Module) {
        let lhs = &module[self.lhs];
        let rhs = &module[self.rhs];

        match self.bin_op {
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
                assert_eq!(self.ty.width(), lhs.width());
                assert_eq!(self.ty.width(), rhs.width());
            }
            BinOp::Eq | BinOp::Ge | BinOp::Gt | BinOp::Le | BinOp::Lt | BinOp::Ne => {
                assert_eq!(lhs.width(), rhs.width());
            }
        }
    }
}

impl MakeNode<BinOpArgs> for BinOpNode {
    fn make(module: &mut Module, args: BinOpArgs) -> NodeId {
        args.assert(module);

        let BinOpArgs {
            bin_op,
            lhs,
            rhs,
            ty,
            sym,
        } = args;

        let node_id = module.add_node(BinOpNode {
            bin_op,
            output: [NodeOutput::wire(ty, sym)],
        });
        module.add_edge(lhs, Port::new(node_id, 0));
        module.add_edge(rhs, Port::new(node_id, 1));

        node_id
    }
}

impl IsNode for BinOpNode {
    #[inline]
    fn in_count(&self) -> usize {
        2
    }

    #[inline]
    fn outputs(&self) -> &[NodeOutput] {
        &self.output
    }

    #[inline]
    fn outputs_mut(&mut self) -> &mut [NodeOutput] {
        &mut self.output
    }
}

pub struct BinOpInputs {
    pub lhs: Port,
    pub rhs: Port,
}

impl WithId<NodeId, &'_ BinOpNode> {
    pub fn inputs(&self, module: &Module) -> BinOpInputs {
        let mut incoming = module.incoming(self.id);

        BinOpInputs {
            lhs: incoming.next(module).unwrap(),
            rhs: incoming.next(module).unwrap(),
        }
    }
}
