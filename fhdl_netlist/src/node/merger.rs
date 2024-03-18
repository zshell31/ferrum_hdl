use std::fmt::Debug;

use super::{IsNode, MakeNode, NodeKind, NodeOutput};
use crate::{
    netlist::{Cursor, CursorMut, Module, NodeId, Port, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Merger {
    pub inputs: u32,
    pub output: [NodeOutput; 1],
    pub rev: bool,
}

#[derive(Debug)]
pub struct MergerArgs<I> {
    pub width: u128,
    pub inputs: I,
    pub rev: bool,
    pub sym: Option<Symbol>,
}

impl<I> MakeNode<MergerArgs<I>> for Merger
where
    I: CursorMut<Item = Port>,
{
    fn make(module: &mut Module, mut args: MergerArgs<I>) -> NodeId {
        let node_id = module.add_node(Merger {
            inputs: 0,
            output: [NodeOutput::wire(NodeTy::BitVec(args.width), args.sym)],
            rev: args.rev,
        });

        let mut inputs = 0;
        let mut width_in = 0;
        while let Some(input) = args.inputs.next(module) {
            module.add_edge(input, Port::new(node_id, inputs));

            width_in += module[input].width();
            inputs += 1;
        }

        assert!(inputs > 0);
        assert_eq!(args.width, width_in);

        if let NodeKind::Merger(merger) = &mut *module[node_id].kind {
            merger.inputs = inputs;
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
        module.incoming(self.id).into_iter(module)
    }
}
