use std::fmt::Debug;

use smallvec::SmallVec;

use super::{IsNode, MakeNode, NodeOutput};
use crate::{
    cursor::Cursor,
    netlist::{Module, NodeId, Port, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Splitter {
    pub outputs: SmallVec<[NodeOutput; 1]>,
    pub start: Option<u128>,
    pub rev: bool,
}

#[derive(Debug)]
pub struct SplitterArgs<O> {
    pub input: Port,
    pub outputs: O,
    pub start: Option<u128>,
    pub rev: bool,
}

fn eval_start(rev: bool, width: u128) -> u128 {
    if !rev {
        0
    } else {
        width
    }
}

impl<O> MakeNode<SplitterArgs<O>> for Splitter
where
    O: IntoIterator<Item = (NodeTy, Option<Symbol>)>,
{
    fn make(module: &mut Module, args: SplitterArgs<O>) -> NodeId {
        let arg_outputs = args.outputs.into_iter();
        let mut outputs = SmallVec::with_capacity(arg_outputs.size_hint().0);

        let width = module[args.input].width();
        let mut start = args.start.unwrap_or_else(|| eval_start(args.rev, width));

        for (ty, sym) in arg_outputs {
            let ty_width = ty.width();
            if !args.rev {
                assert!(
                    start + ty_width <= width,
                    "Invalid inputs/outputs for splitter"
                );
                start += ty_width;
            } else {
                assert!(start >= ty_width, "Invalid inputs/outputs for splitter");
                start -= ty_width;
            }

            outputs.push(NodeOutput::wire(ty, sym))
        }
        assert!(!outputs.is_empty());

        let node_id = module.add_node(Splitter {
            outputs,
            start: args.start,
            rev: args.rev,
        });

        module.add_edge(args.input, Port::new(node_id, 0));

        node_id
    }
}

impl IsNode for Splitter {
    #[inline]
    fn in_count(&self) -> usize {
        1
    }

    #[inline]
    fn outputs(&self) -> &[NodeOutput] {
        &self.outputs
    }

    #[inline]
    fn outputs_mut(&mut self) -> &mut [NodeOutput] {
        &mut self.outputs
    }
}

pub type Indices<'n> = impl Iterator<Item = (u128, &'n NodeOutput)> + 'n;

impl WithId<NodeId, &'_ Splitter> {
    pub fn input(&self, module: &Module) -> Port {
        let mut incoming = module.incoming(self.id);
        Cursor::next_(&mut incoming, module).unwrap()
    }

    pub fn eval_indices<'n>(&'n self, module: &Module) -> Indices<'n> {
        let input = self.input(module);
        let width = module[input].width();

        let rev = self.rev;
        let mut start = self.start.unwrap_or_else(|| eval_start(rev, width));

        self.outputs().map(move |output| {
            let width = output.width();

            if !rev {
                let res = (start, output.inner);
                start += width;
                res
            } else {
                start -= width;
                (start, output.inner)
            }
        })
    }

    pub fn pass_all_bits(&self, module: &Module) -> bool {
        if self.out_count() != 1 {
            return false;
        }

        let input = self.input(module);
        let in_width = module[input].width();
        let out_width = self.outputs[0].ty.width();

        in_width == out_width
    }
}
