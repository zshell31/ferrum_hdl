use std::borrow::Cow;

use derive_where::derive_where;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{buffer::Buffer, net_list::NodeOutId, sig_ty::PrimTy, symbol::Symbol};

#[derive_where(Debug)]
pub struct Expr {
    pub input: NodeOutId,
    pub output: NodeOutput,
    pub skip_output_def: bool,
    // TODO: how to specify trans for different backends (Verilog, VHDL, etc)
    #[allow(clippy::type_complexity)]
    #[derive_where(skip)]
    pub expr: Box<dyn Fn(&mut Buffer, Cow<'static, str>, Symbol)>,
}

impl Expr {
    pub fn new(
        ty: PrimTy,
        input: NodeOutId,
        sym: Symbol,
        skip_output_def: bool,
        expr: impl Fn(&mut Buffer, Cow<'static, str>, Symbol) + 'static,
    ) -> Self {
        Self {
            input,
            output: NodeOutput::wire(ty, sym),
            skip_output_def,
            expr: Box::new(expr),
        }
    }
}

impl From<Expr> for NodeKind {
    fn from(node: Expr) -> Self {
        Self::Expr(node)
    }
}

impl IsNode for Expr {
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
