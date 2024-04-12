use petgraph::{
    algo::dominators::{simple_fast, Dominators},
    prelude::NodeIndex,
    stable_graph::IndexType,
    Directed, Graph,
};
use rustc_data_structures::graph::WithSuccessors;
use rustc_middle::mir::{BasicBlock, BasicBlocks, TerminatorKind, START_BLOCK};

use super::{Compiler, Context};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
struct BasicBlockWrap(BasicBlock);

impl Default for BasicBlockWrap {
    fn default() -> Self {
        Self(START_BLOCK)
    }
}

impl From<NodeIndex<BasicBlockWrap>> for BasicBlockWrap {
    fn from(idx: NodeIndex<BasicBlockWrap>) -> Self {
        BasicBlockWrap::new(idx.index())
    }
}

unsafe impl IndexType for BasicBlockWrap {
    fn new(x: usize) -> Self {
        Self(BasicBlock::from_usize(x))
    }

    fn index(&self) -> usize {
        self.0.index()
    }

    fn max() -> Self {
        Self(BasicBlock::MAX)
    }
}

#[derive(Debug)]
pub struct PostDominator(Dominators<NodeIndex<BasicBlockWrap>>);

impl PostDominator {
    fn new(blocks: &BasicBlocks) -> Self {
        let (root, graph) = Self::create_rev_graph(blocks);
        let post_dominator = simple_fast(&graph, root);

        Self(post_dominator)
    }

    fn immediate_post_dominator(&self, block: BasicBlock) -> Option<BasicBlock> {
        self.0
            .immediate_dominator(NodeIndex::<_>::from(BasicBlockWrap(block)))
            .map(BasicBlockWrap::from)
            .map(|wrap| wrap.0)
    }

    fn create_rev_graph(
        basic_blocks: &BasicBlocks,
    ) -> (
        NodeIndex<BasicBlockWrap>,
        Graph<(), (), Directed, BasicBlockWrap>,
    ) {
        let mut graph = Graph::default();
        let mut root = None;

        for (block, block_data) in basic_blocks.iter_enumerated() {
            if let TerminatorKind::Return = block_data.terminator().kind {
                assert!(root.is_none());
                root = Some(NodeIndex::<_>::from(BasicBlockWrap(block)));
            }
            let idx = graph.add_node(());
            let idx = BasicBlockWrap::from(idx);
            assert_eq!(idx.0, block);
        }

        for (block, _) in basic_blocks.iter_enumerated() {
            let to: NodeIndex<_> = BasicBlockWrap(block).into();
            for successor in basic_blocks.successors(block) {
                let from: NodeIndex<_> = BasicBlockWrap(successor).into();
                graph.add_edge(from, to, ());
            }
        }

        (root.unwrap(), graph)
    }
}

impl<'tcx> Compiler<'tcx> {
    pub fn find_convergent_block(
        &mut self,
        block: BasicBlock,
        ctx: &Context<'tcx>,
    ) -> Option<BasicBlock> {
        self.post_dominator
            .entry(ctx.fn_did)
            .or_insert_with(|| PostDominator::new(&ctx.mir.basic_blocks))
            .immediate_post_dominator(block)
    }
}
