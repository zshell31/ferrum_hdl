use either::Either;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    arena::with_arena, net_list::NodeOutId, params::Inputs, sig_ty::PrimTy,
    symbol::Symbol,
};

#[derive(Debug, Clone)]
pub struct DFF {
    pub inputs: &'static DFFInputs,
    pub output: NodeOutput,
}

impl DFF {
    pub fn new(
        ty: PrimTy,
        clk: NodeOutId,
        rst: NodeOutId,
        en: Option<NodeOutId>,
        rst_val: NodeOutId,
        data: NodeOutId,
        sym: Symbol,
    ) -> Self {
        Self {
            inputs: unsafe {
                with_arena().alloc(DFFInputs {
                    clk,
                    rst,
                    en,
                    rst_val,
                    data,
                })
            },
            output: NodeOutput::reg(ty, sym),
        }
    }
}

impl From<DFF> for NodeKind {
    fn from(node: DFF) -> Self {
        Self::DFF(node)
    }
}

impl IsNode for DFF {
    type Inputs = DFFInputs;
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

#[derive(Debug, Clone, Copy)]
pub struct DFFInputs {
    pub clk: NodeOutId,
    pub rst: NodeOutId,
    pub en: Option<NodeOutId>,
    pub rst_val: NodeOutId,
    pub data: NodeOutId,
}

impl Inputs for DFFInputs {
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        match self.en {
            Some(en) => Either::Left(
                [self.clk, self.rst, en, self.rst_val, self.data].into_iter(),
            ),
            None => {
                Either::Right([self.clk, self.rst, self.rst_val, self.data].into_iter())
            }
        }
    }

    fn len(&self) -> usize {
        match self.en {
            Some(_) => 5,
            None => 4,
        }
    }
}
