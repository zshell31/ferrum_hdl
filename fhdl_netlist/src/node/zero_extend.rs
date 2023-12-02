use rustc_macros::{Decodable, Encodable};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NetList, NodeOutId, NodeOutIdx, WithId},
    resolver::{Resolve, Resolver},
    sig_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, Encodable, Decodable)]
pub struct ZeroExtend {
    input: NodeOutIdx,
    output: NodeOutput,
}

impl ZeroExtend {
    pub fn new(ty: NodeTy, input: NodeOutId, sym: impl Into<Option<Symbol>>) -> Self {
        Self {
            input: input.into(),
            output: NodeOutput::wire(ty, sym.into()),
        }
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl WithId<ModuleId, &'_ ZeroExtend> {
    pub fn input(&self) -> NodeOutId {
        NodeOutId::make(self.id(), self.input)
    }
}

impl<R: Resolver> Resolve<R> for ZeroExtend {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(Self {
            input: self.input,
            output: self.output.resolve(resolver)?,
        })
    }
}

impl From<ZeroExtend> for NodeKind {
    fn from(node: ZeroExtend) -> Self {
        Self::ZeroExtend(node)
    }
}

impl IsNode for ZeroExtend {
    type Inputs = NodeOutIdx;
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &self.input
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut self.input
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }

    fn validate(&self, module_id: ModuleId, net_list: &NetList) {
        if let (Some(input_width), Some(output_width)) = (
            net_list[NodeOutId::make(module_id, self.input)]
                .width()
                .opt_value(),
            self.output.width().opt_value(),
        ) {
            if input_width > output_width {
                panic!(
                    "ZeroExtend: output width {} < input width {}",
                    output_width, input_width
                );
            }
        }
    }
}
