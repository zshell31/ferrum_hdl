use std::fmt::Debug;

use super::{IsNode, MakeNode, NodeOutput};
use crate::{netlist::Module, node_ty::NodeTy, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct Input {
    pub output: [NodeOutput; 1],
}

pub struct InputArgs {
    pub ty: NodeTy,
    pub sym: Option<Symbol>,
}

impl MakeNode<InputArgs> for Input {
    fn make(module: &mut Module, args: InputArgs) -> crate::netlist::NodeId {
        let InputArgs { ty, sym } = args;
        module.add_node(Input {
            output: [NodeOutput::wire(ty, sym)],
        })
    }
}

impl IsNode for Input {
    #[inline]
    fn in_count(&self) -> usize {
        0
    }

    #[inline]
    fn out_count(&self) -> usize {
        1
    }

    #[inline]
    fn outputs(&self) -> &[NodeOutput] {
        &self.output
    }

    #[inline]
    fn outputs_mut(&mut self) -> &mut [NodeOutput] {
        &mut self.output
    }
}
