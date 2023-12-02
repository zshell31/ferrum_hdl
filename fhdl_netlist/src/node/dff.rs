use rustc_macros::{Decodable, Encodable};
use smallvec::SmallVec;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    encoding::Wrap,
    net_list::{ModuleId, NodeOutId, NodeOutIdx, WithId},
    resolver::{Resolve, Resolver},
    sig_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct DFF {
    inputs: Wrap<SmallVec<[NodeOutIdx; 5]>>,
    en_idx: u8,
    data_idx: u8,
    output: NodeOutput,
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
        let mut inputs: SmallVec<_> =
            [clk, rst, rst_val].into_iter().map(Into::into).collect();
        let mut en_idx = 0;
        if let Some(en) = en {
            en_idx = inputs.len() as u8;
            inputs.push(en.into());
        }
        let mut data_idx = 0;
        if let Some(data) = data {
            data_idx = inputs.len() as u8;
            inputs.push(data.into());
        }

        Self {
            inputs: inputs.into(),
            en_idx,
            data_idx,
            output: NodeOutput::reg(ty, sym.into(), 2),
        }
    }

    pub(crate) fn set_data(&mut self, data: NodeOutId) -> usize {
        let data_idx = self.inputs.len();
        self.data_idx = data_idx as u8;
        self.inputs.push(data.into());
        data_idx
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl<R: Resolver> Resolve<R> for DFF {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(Self {
            inputs: self.inputs.clone(),
            en_idx: self.en_idx,
            data_idx: self.data_idx,
            output: self.output.resolve(resolver)?,
        })
    }
}

impl WithId<ModuleId, &'_ DFF> {
    pub fn inputs(&self) -> DFFInputs {
        let module_id = self.id();

        DFFInputs {
            clk: NodeOutId::make(module_id, self.inputs[0]),
            rst: NodeOutId::make(module_id, self.inputs[1]),
            en: if self.en_idx != 0 {
                Some(NodeOutId::make(
                    module_id,
                    self.inputs[self.en_idx as usize],
                ))
            } else {
                None
            },
            rst_val: NodeOutId::make(module_id, self.inputs[2]),
            data: if self.data_idx != 0 {
                NodeOutId::make(module_id, self.inputs[self.data_idx as usize])
            } else {
                panic!("no data input specified")
            },
        }
    }
}

impl From<DFF> for NodeKind {
    fn from(node: DFF) -> Self {
        Self::DFF(node)
    }
}

impl IsNode for DFF {
    type Inputs = [NodeOutIdx];
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
