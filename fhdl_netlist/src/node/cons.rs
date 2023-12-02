use rustc_macros::{Decodable, Encodable};
use smallvec::SmallVec;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    encoding::Wrap,
    net_list::NodeOutIdx,
    resolver::{Resolve, Resolver},
    sig_ty::{NodeTy, Width},
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, Encodable, Decodable)]
pub struct Const {
    value: Width,
    output: NodeOutput,
}

impl Const {
    pub fn new(ty: NodeTy, value: Width, sym: Option<Symbol>) -> Self {
        Self {
            value,
            output: NodeOutput::wire(ty, sym),
        }
    }

    pub fn value(&self) -> &Width {
        &self.value
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }
}

impl<R: Resolver> Resolve<R> for Const {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(Self {
            value: self.value.resolve(resolver)?,
            output: self.output.resolve(resolver)?,
        })
    }
}

impl From<Const> for NodeKind {
    fn from(node: Const) -> Self {
        Self::Const(node)
    }
}

impl IsNode for Const {
    type Inputs = [NodeOutIdx];
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &[]
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut []
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct MultiConst {
    values: Wrap<SmallVec<[u128; 1]>>,
    outputs: Wrap<SmallVec<[NodeOutput; 1]>>,
}

impl MultiConst {
    pub fn new(
        values: impl IntoIterator<Item = u128>,
        outputs: impl IntoIterator<Item = NodeOutput>,
    ) -> Self {
        Self {
            values: values.into_iter().collect(),
            outputs: outputs.into_iter().collect(),
        }
    }

    pub fn values(&self) -> &[u128] {
        &self.values
    }

    pub fn outputs(&self) -> &[NodeOutput] {
        &self.outputs
    }
}

impl<R: Resolver> Resolve<R> for MultiConst {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(Self {
            values: self.values.resolve(resolver)?,
            outputs: self.outputs.resolve(resolver)?,
        })
    }
}

impl From<MultiConst> for NodeKind {
    fn from(node: MultiConst) -> Self {
        Self::MultiConst(node)
    }
}

impl IsNode for MultiConst {
    type Inputs = [NodeOutIdx];
    type Outputs = [NodeOutput];

    fn inputs(&self) -> &Self::Inputs {
        &[]
    }
    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        &mut []
    }

    fn outputs(&self) -> &Self::Outputs {
        self.outputs.as_slice()
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        self.outputs.as_mut_slice()
    }
}
