use rustc_macros::{Decodable, Encodable};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{NetList, NodeOutId},
    sig_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, Encodable, Decodable)]
pub struct ZeroExtend {
    pub input: NodeOutId,
    pub output: NodeOutput,
}

impl ZeroExtend {
    pub fn new(ty: NodeTy, input: NodeOutId, sym: impl Into<Option<Symbol>>) -> Self {
        Self {
            input,
            output: NodeOutput::wire(ty, sym.into()),
        }
    }
}

impl From<ZeroExtend> for NodeKind {
    fn from(node: ZeroExtend) -> Self {
        Self::ZeroExtend(node)
    }
}

impl IsNode for ZeroExtend {
    type Inputs = NodeOutId;
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

    fn validate(&self, net_list: &NetList) {
        if let (Some(input_width), Some(output_width)) = (
            net_list[self.input].width().opt_value(),
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
