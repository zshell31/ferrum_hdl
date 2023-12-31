use rustc_macros::{Decodable, Encodable};
use smallvec::SmallVec;

use super::{assert_width, IsNode, NodeKind, NodeOutput};
use crate::{
    encoding::Wrap,
    net_list::{ModuleId, NetList, NodeOutId, NodeOutIdx, WithId},
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
            output: NodeOutput::reg(ty, sym.into(), Some(2)),
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

    fn clk(&self) -> NodeOutIdx {
        self.inputs[0]
    }

    fn rst(&self) -> NodeOutIdx {
        self.inputs[1]
    }

    fn rst_val(&self) -> NodeOutIdx {
        self.inputs[2]
    }

    fn en(&self) -> Option<NodeOutIdx> {
        if self.en_idx != 0 {
            Some(self.inputs[self.en_idx as usize])
        } else {
            None
        }
    }

    fn data(&self) -> Option<NodeOutIdx> {
        if self.data_idx != 0 {
            Some(self.inputs[self.data_idx as usize])
        } else {
            None
        }
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
            clk: NodeOutId::make(module_id, self.clk()),
            rst: NodeOutId::make(module_id, self.rst()),
            en: self.en().map(|en| NodeOutId::make(module_id, en)),
            rst_val: NodeOutId::make(module_id, self.rst_val()),
            data: NodeOutId::make(
                module_id,
                self.data().expect("no data input specified"),
            ),
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

    fn assert(&self, module_id: ModuleId, net_list: &NetList) {
        let node = WithId::<ModuleId, _>::new(module_id, self);
        let inputs = node.inputs();

        let data = &net_list[inputs.data];
        assert_width!(self.output.width(), data.width());

        let rst_val = &net_list[inputs.rst_val];
        assert_width!(self.output.width(), rst_val.width());
    }
}
