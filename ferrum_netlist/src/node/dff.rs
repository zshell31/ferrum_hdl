use either::Either;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, params::Inputs, sig_ty::PrimTy, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct DFF {
    pub inputs: DFFInputs,
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
            inputs: DFFInputs {
                clk,
                rst,
                en,
                rst_val,
                data,
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

    fn items_mut(&mut self) -> impl Iterator<Item = &mut NodeOutId> + '_ {
        match &mut self.en {
            Some(en) => Either::Left(
                [
                    &mut self.clk,
                    &mut self.rst,
                    en,
                    &mut self.rst_val,
                    &mut self.data,
                ]
                .into_iter(),
            ),
            None => Either::Right(
                [
                    &mut self.clk,
                    &mut self.rst,
                    &mut self.rst_val,
                    &mut self.data,
                ]
                .into_iter(),
            ),
        }
    }

    fn len(&self) -> usize {
        match self.en {
            Some(_) => 5,
            None => 4,
        }
    }
}
