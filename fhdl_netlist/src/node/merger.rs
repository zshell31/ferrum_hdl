use std::fmt::Debug;

use rustc_macros::{Decodable, Encodable};

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NodeOutId, NodeOutIdx, WithId},
    resolver::{Resolve, Resolver},
    sig_ty::{NodeTy, Width},
    symbol::Symbol,
};

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct Merger {
    inputs: Vec<NodeOutIdx>,
    output: NodeOutput,
    rev: bool,
}

impl Merger {
    pub fn new(
        width: Width,
        inputs: impl IntoIterator<Item = NodeOutId>,
        rev: bool,
        sym: impl Into<Option<Symbol>>,
    ) -> Self {
        Self {
            inputs: inputs.into_iter().map(Into::into).collect(),
            output: NodeOutput::wire(NodeTy::BitVec(width), sym.into()),
            rev,
        }
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }

    pub fn rev(&self) -> bool {
        self.rev
    }

    pub fn inputs_len(&self) -> usize {
        self.inputs.len()
    }

    pub fn inputs_is_empty(&self) -> bool {
        self.inputs.is_empty()
    }
}

impl WithId<ModuleId, &'_ Merger> {
    pub fn inputs(&self) -> impl DoubleEndedIterator<Item = NodeOutId> + '_ {
        let module_id = self.id();
        self.inputs
            .iter()
            .map(move |input| NodeOutId::make(module_id, *input))
    }
}

impl<R: Resolver> Resolve<R> for Merger {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(Self {
            inputs: self.inputs.clone(),
            output: self.output.resolve(resolver)?,
            rev: self.rev,
        })
    }
}

impl From<Merger> for NodeKind {
    fn from(node: Merger) -> Self {
        Self::Merger(node)
    }
}

impl IsNode for Merger {
    type Inputs = [NodeOutIdx];
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        self.inputs.as_slice()
    }

    fn inputs_mut(&mut self) -> &mut Self::Inputs {
        self.inputs.as_mut_slice()
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}
