use smallvec::SmallVec;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{net_list::NodeOutId, sig_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone)]
pub struct DFF {
    inputs: SmallVec<[NodeOutId; 5]>,
    en_idx: u8,
    data_idx: u8,
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
        data: Option<NodeOutId>,
        sym: impl Into<Option<Symbol>>,
    ) -> Self {
        let mut inputs: SmallVec<_> = [clk, rst, rst_val].into_iter().collect();
        let mut en_idx = 0;
        if let Some(en) = en {
            en_idx = inputs.len() as u8;
            inputs.push(en);
        }
        let mut data_idx = 0;
        if let Some(data) = data {
            data_idx = inputs.len() as u8;
            inputs.push(data);
        }

        Self {
            inputs,
            en_idx,
            data_idx,
            output: NodeOutput::reg(ty, sym.into(), 2),
        }
    }

    pub fn dff_inputs(&self) -> DFFInputs {
        DFFInputs {
            clk: self.inputs[0],
            rst: self.inputs[1],
            en: if self.en_idx != 0 {
                Some(self.inputs[self.en_idx as usize])
            } else {
                None
            },
            rst_val: self.inputs[2],
            data: if self.data_idx != 0 {
                self.inputs[self.data_idx as usize]
            } else {
                panic!("no data input specified")
            },
        }
    }

    pub(crate) fn set_data(&mut self, data: NodeOutId) -> usize {
        let data_idx = self.inputs.len();
        self.data_idx = data_idx as u8;
        self.inputs.push(data);
        data_idx
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
