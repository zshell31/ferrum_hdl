use std::fmt::Debug;

use fhdl_data_structures::{
    cursor::Cursor,
    graph::{NodeId, Port},
};

use super::{IsNode, MakeNode, NodeKind, NodeOutput};
use crate::{netlist::Module, node_ty::NodeTy, symbol::Symbol, with_id::WithId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Merger {
    pub inputs: u32,
    pub output: [NodeOutput; 1],
    pub rev: bool,
}

#[derive(Debug)]
pub struct MergerArgs<I> {
    pub inputs: I,
    pub rev: bool,
    pub sym: Option<Symbol>,
}

impl<I> MakeNode<MergerArgs<I>> for Merger
where
    I: IntoIterator<Item = Port>,
{
    fn make(module: &mut Module, args: MergerArgs<I>) -> NodeId {
        let node_id = module.add_node(Merger {
            inputs: 0,
            output: [NodeOutput::wire(NodeTy::BitVec(0), args.sym)],
            rev: args.rev,
        });

        let mut inputs = 0;
        let mut width = 0;
        for input in args.inputs {
            module.add_edge(input, Port::new(node_id, inputs));

            width += module[input].width();
            inputs += 1;
        }

        assert!(inputs > 0);

        if let NodeKind::Merger(merger) = module[node_id].kind_mut() {
            merger.inputs = inputs;
            merger.output[0].ty = NodeTy::BitVec(width);
        }

        node_id
    }
}

impl IsNode for Merger {
    #[inline]
    fn in_count(&self) -> usize {
        self.inputs as usize
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

impl WithId<NodeId, &'_ Merger> {
    pub fn inputs<'m>(&self, module: &'m Module) -> impl Iterator<Item = Port> + 'm {
        module.incoming(self.id).into_iter_(module)
    }
}
