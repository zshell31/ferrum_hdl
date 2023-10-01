use std::{fmt::Debug, iter};

use either::Either;

use super::{IsNode, NodeKind, NodeOutput};
use crate::{
    arena::with_arena, net_list::NodeOutId, params::Inputs, sig_ty::PrimTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, Default)]
pub struct BitVecMask {
    pub val: u128,
    pub mask: u128,
}

impl BitVecMask {
    pub fn set_val(&mut self, val: u128, width: u128) {
        self.val |= val & ((1 << width) - 1);
    }

    pub fn set_mask(&mut self, width: u128) {
        self.mask |= (1 << width) - 1;
    }

    pub fn shiftl(&mut self, width: u128) {
        self.val <<= width;
        self.mask <<= width;
    }

    pub fn shiftr(&mut self, width: u128) {
        self.val >>= width;
        self.mask >>= width;
    }

    pub fn to_bitstr(&self, width: u128, wildcard: char) -> String {
        let width = width as usize;

        let mut mask = 1 << (width - 1);
        (0 .. width)
            .map(|_| {
                let ch = if (self.mask & mask) != 0 {
                    wildcard
                } else if (self.val & mask) != 0 {
                    '1'
                } else {
                    '0'
                };

                mask >>= 1;
                ch
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct Case {
    pub inputs: (
        NodeOutId,
        &'static [(BitVecMask, NodeOutId)],
        Option<NodeOutId>,
    ),
    pub output: NodeOutput,
}

impl Case {
    pub fn new(
        ty: PrimTy,
        sel: NodeOutId,
        inputs: impl IntoIterator<Item = (BitVecMask, NodeOutId)>,
        default: Option<NodeOutId>,
        sym: Symbol,
    ) -> Self {
        Self {
            inputs: (
                sel,
                unsafe { with_arena().alloc_from_iter(inputs) },
                default,
            ),
            output: NodeOutput::wire(ty, sym),
        }
    }
}

impl From<Case> for NodeKind {
    fn from(node: Case) -> Self {
        Self::Case(node)
    }
}

impl IsNode for Case {
    type Inputs = (
        NodeOutId,
        &'static [(BitVecMask, NodeOutId)],
        Option<NodeOutId>,
    );
    type Outputs = NodeOutput;

    fn inputs(&self) -> &Self::Inputs {
        &self.inputs
    }

    fn outputs(&self) -> &Self::Outputs {
        &self.output
    }

    fn outputs_mut(&mut self) -> &mut Self::Outputs {
        &mut self.output
    }
}

impl Inputs
    for (
        NodeOutId,
        &'static [(BitVecMask, NodeOutId)],
        Option<NodeOutId>,
    )
{
    fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        let outs =
            iter::once(self.0).chain(self.1.iter().map(|(_, node_out_id)| *node_out_id));

        match self.2 {
            Some(node_out_id) => Either::Left(outs.chain(iter::once(node_out_id))),
            None => Either::Right(outs),
        }
    }

    fn len(&self) -> usize {
        let len = self.1.len() + 1;
        match self.2 {
            Some(_) => len + 1,
            None => len,
        }
    }
}
