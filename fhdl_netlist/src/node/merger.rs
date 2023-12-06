use std::fmt::Debug;

use rustc_macros::{Decodable, Encodable};

use super::{assert_opt, IsNode, NodeKind, NodeOutput};
use crate::{
    net_list::{ModuleId, NetList, NodeOutId, NodeOutIdx, WithId},
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
    pub fn new<I>(
        width: Width,
        inputs: I,
        rev: bool,
        sym: impl Into<Option<Symbol>>,
    ) -> Self
    where
        I: IntoIterator<Item = NodeOutId>,
        I::IntoIter: DoubleEndedIterator,
    {
        let inputs = inputs.into_iter().map(Into::into);
        let inputs = if !rev {
            inputs.collect()
        } else {
            inputs.rev().collect()
        };

        Self {
            inputs,
            output: NodeOutput::wire(NodeTy::BitVec(width), sym.into()),
            rev,
        }
    }

    pub fn output(&self) -> &NodeOutput {
        &self.output
    }

    pub fn inputs_len(&self) -> usize {
        self.inputs.len()
    }

    pub fn inputs_is_empty(&self) -> bool {
        self.inputs.is_empty()
    }

    pub fn rev(&self) -> bool {
        self.rev
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

    fn assert(&self, module_id: ModuleId, net_list: &NetList) {
        let node = WithId::<ModuleId, _>::new(module_id, self);
        let total = node
            .inputs()
            .map(|input| net_list[input].width().opt_value())
            .sum::<Option<u128>>();

        assert_opt!(total, self.output.width().opt_value());
    }
}
