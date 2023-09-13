use super::{IsNode, Node, NodeOutput};
use crate::{net_kind::NetKind, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug)]
pub struct LoopStart {
    pub genvar: Symbol,
    pub count: u128,
    pub output: Option<NodeOutput>,
}

impl LoopStart {
    pub fn new(genvar: Symbol, count: u128, out: Option<(PrimTy, Symbol)>) -> Self {
        Self {
            genvar,
            count,
            output: out.map(|(ty, sym)| NodeOutput {
                ty,
                sym,
                kind: NetKind::Wire,
            }),
        }
    }

    pub fn set_out(&mut self, out: Option<(PrimTy, Symbol)>) {
        self.output = out.map(|(ty, sym)| NodeOutput {
            ty,
            sym,
            kind: NetKind::Wire,
        });
    }
}

impl From<LoopStart> for Node {
    fn from(node: LoopStart) -> Self {
        Self::LoopStart(node)
    }
}

impl IsNode for LoopStart {
    type Inputs = ();
    type Outputs = Option<NodeOutput>;

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

#[derive(Debug)]
pub struct LoopEnd {}

impl From<LoopEnd> for Node {
    fn from(node: LoopEnd) -> Self {
        Self::LoopEnd(node)
    }
}

impl IsNode for LoopEnd {
    type Inputs = ();
    type Outputs = ();

    fn inputs(&self) -> &Self::Inputs {
        &()
    }

    fn outputs(&self) -> &Self::Outputs {
        &()
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        unreachable!()
    }
}
