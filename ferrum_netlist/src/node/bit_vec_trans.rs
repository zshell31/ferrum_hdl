use derive_where::derive_where;

use super::{IsNode, Node, NodeOutput};
use crate::{
    buffer::Buffer, net_kind::NetKind, net_list::NodeOutId, sig_ty::PrimTy,
    symbol::Symbol,
};

#[derive_where(Debug)]
pub struct BitVecTrans {
    pub input: NodeOutId,
    pub output: NodeOutput,
    // TODO: how to specify trans for different backends (Verilog, VHDL, etc)
    #[allow(clippy::type_complexity)]
    #[derive_where(skip)]
    pub trans: Box<dyn Fn(&mut Buffer, Symbol, Symbol)>,
}

impl BitVecTrans {
    pub fn new(
        width: u128,
        input: NodeOutId,
        sym: Symbol,
        trans: impl Fn(&mut Buffer, Symbol, Symbol) + 'static,
    ) -> Self {
        Self {
            input,
            output: NodeOutput {
                ty: PrimTy::BitVec(width),
                sym,
                kind: NetKind::Wire,
            },
            trans: Box::new(trans),
        }
    }
}

impl From<BitVecTrans> for Node {
    fn from(node: BitVecTrans) -> Self {
        Self::BitVecTrans(node)
    }
}

impl IsNode for BitVecTrans {
    type Inputs = NodeOutId;
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &self.input
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}