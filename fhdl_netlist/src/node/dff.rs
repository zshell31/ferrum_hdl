use smallvec::SmallVec;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, sig_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct DFF {
    inputs: SmallVec<[NodeOutId; 5]>,
    is_en: bool,
    pub output: NodeOutput,
}

#[derive(Debug, Clone, Copy)]
pub struct DFFInputs {
    pub clk: NodeOutId,
    pub rst: NodeOutId,
    pub en: Option<NodeOutId>,
    pub rst_val: NodeOutId,
    pub data: NodeOutId,
}

impl DFF {
    pub fn new(
        ty: NodeTy,
        clk: NodeOutId,
        rst: NodeOutId,
        en: Option<NodeOutId>,
        rst_val: NodeOutId,
        data: NodeOutId,
        sym: impl Into<Option<Symbol>>,
    ) -> Self {
        let mut inputs: SmallVec<_> = [clk, rst, rst_val, data].into_iter().collect();
        let is_en = en.is_some();
        if let Some(en) = en {
            inputs.push(en);
        }
        Self {
            inputs,
            is_en,
            output: NodeOutput::reg(ty, sym.into(), rst_val),
        }
    }

    pub fn dff_inputs(&self) -> DFFInputs {
        DFFInputs {
            clk: self.inputs[0],
            rst: self.inputs[1],
            en: if self.is_en {
                Some(self.inputs[4])
            } else {
                None
            },
            rst_val: self.inputs[2],
            data: self.inputs[3],
        }
    }
}

impl From<DFF> for NodeKind {
    fn from(node: DFF) -> Self {
        Self::DFF(node)
    }
}

impl IsNode for DFF {
    type Inputs = [NodeOutId];
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
