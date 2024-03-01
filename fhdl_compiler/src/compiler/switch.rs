use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    iter,
    rc::Rc,
};

use fhdl_netlist::{
    const_val::ConstVal,
    netlist::IterMut,
    node::{Mux, MuxArgs, Splitter, SplitterArgs},
};
use petgraph::{
    algo::dominators::{simple_fast, Dominators},
    prelude::NodeIndex,
    stable_graph::IndexType,
    Directed, Graph,
};
use rustc_data_structures::{
    fx::{FxHashMap, FxHashSet},
    graph::WithSuccessors,
};
use rustc_hir::def_id::DefId;
use rustc_middle::{
    mir::{
        BasicBlock, BasicBlocks, Local, Operand, SwitchTargets, TerminatorKind,
        START_BLOCK,
    },
    ty::Ty,
};
use rustc_span::Span;
use tracing::{debug, error, instrument};

use super::{
    item::{Group, Item, ModuleExt},
    item_ty::ItemTyKind,
    Compiler, Context, SymIdent,
};
use crate::error::{Error, SpanError, SpanErrorKind};

#[derive(Debug, Clone)]
pub struct Branch<'tcx> {
    pub discr: u128,
    pub locals: BTreeMap<Local, Item<'tcx>>,
}

impl<'tcx> Branch<'tcx> {
    fn new(discr: u128) -> Self {
        Self {
            discr,
            locals: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Switch<'tcx> {
    pub locals: Rc<BTreeSet<Local>>,
    pub discr: Item<'tcx>,
    targets: usize,
    branches: Vec<Branch<'tcx>>,
    is_otherwise: bool,
    otherwise: Option<BTreeMap<Local, Item<'tcx>>>,
}

impl<'tcx> Switch<'tcx> {
    pub fn new(locals: Rc<BTreeSet<Local>>, discr: Item<'tcx>, targets: usize) -> Self {
        Self {
            locals,
            discr,
            targets,
            branches: Vec::with_capacity(targets),
            is_otherwise: false,
            otherwise: None,
        }
    }

    pub fn contains(&self, local: Local) -> bool {
        self.locals.contains(&local)
    }

    pub fn add_branch(&mut self, discr: Option<u128>) {
        if let Some(discr) = discr {
            self.branches.push(Branch::new(discr));
            self.is_otherwise = false;
        } else {
            self.otherwise = Some(Default::default());
            self.is_otherwise = true;
        }
    }

    pub fn add_branch_local(&mut self, local: Local, item: Item<'tcx>) {
        assert!(self.locals.contains(&local));
        if self.is_otherwise {
            self.otherwise.as_mut().unwrap().insert(local, item);
        } else {
            self.branches.last_mut().unwrap().locals.insert(local, item);
        }
    }

    pub fn branch_local(&self, local: Local) -> Option<&Item<'tcx>> {
        assert!(self.locals.contains(&local));
        if self.is_otherwise {
            self.otherwise.as_ref().unwrap().get(&local)
        } else {
            self.branches.last().unwrap().locals.get(&local)
        }
    }

    fn branches_len(&self) -> usize {
        self.branches.len() + self.otherwise.is_some() as usize
    }

    pub fn branches(&self) -> impl DoubleEndedIterator<Item = &Branch<'tcx>> {
        assert_eq!(self.targets, self.branches_len());
        self.branches.iter().map(|branch| {
            assert_eq!(self.locals.len(), branch.locals.len());
            for local in self.locals.iter() {
                assert!(branch.locals.contains_key(local));
            }

            branch
        })
    }

    pub fn otherwise(&self) -> Option<&BTreeMap<Local, Item<'tcx>>> {
        let otherwise = self.otherwise.as_ref()?;
        assert_eq!(self.locals.len(), otherwise.len());
        for local in self.locals.iter() {
            assert!(otherwise.contains_key(local));
        }

        self.otherwise.as_ref()
    }
}

struct BranchBlocks<'b, 'tcx: 'b> {
    stop: BasicBlock,
    basic_blocks: &'b BasicBlocks<'tcx>,
    to_visit: VecDeque<BasicBlock>,
}

impl<'b, 'tcx: 'b> BranchBlocks<'b, 'tcx> {
    fn new(
        basic_blocks: &'b BasicBlocks<'tcx>,
        branch: BasicBlock,
        stop: BasicBlock,
    ) -> Self {
        Self {
            stop,
            basic_blocks,
            to_visit: {
                let mut res = VecDeque::new();
                res.push_back(branch);
                res
            },
        }
    }
}

impl<'b, 'tcx: 'b> Iterator for BranchBlocks<'b, 'tcx> {
    type Item = BasicBlock;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(block) = self.to_visit.pop_front() {
            if block == self.stop {
                continue;
            }

            self.to_visit.extend(self.basic_blocks.successors(block));

            return Some(block);
        }

        None
    }
}

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

pub(super) struct SwitchBlocks {
    post_dominators: Dominators<NodeIndex<BasicBlockWrap>>,
    switch_meta: FxHashMap<BasicBlock, SwitchMeta>,
}

impl SwitchBlocks {
    fn make(basic_blocks: &BasicBlocks) -> Self {
        let (root, graph) = create_rev_graph(basic_blocks);
        let post_dominators = simple_fast(&graph, root);

        Self {
            post_dominators,
            switch_meta: Default::default(),
        }
    }

    fn immediate_post_dominator(&self, block: BasicBlock) -> Option<BasicBlock> {
        self.post_dominators
            .immediate_dominator(NodeIndex::<_>::from(BasicBlockWrap(block)))
            .map(BasicBlockWrap::from)
            .map(|wrap| wrap.0)
    }

    fn switch_meta(
        &mut self,
        switch_block: BasicBlock,
        basic_blocks: &BasicBlocks,
        targets: &[BasicBlock],
    ) -> Option<&SwitchMeta> {
        #[allow(clippy::map_entry)]
        if !self.switch_meta.contains_key(&switch_block) {
            let convergent_block = match self.immediate_post_dominator(switch_block) {
                Some(convergent_block) => convergent_block,
                None => {
                    error!("Cannot find convergent block for switchInt targets");
                    return None;
                }
            };

            self.switch_meta.insert(
                switch_block,
                SwitchMeta::make(basic_blocks, targets, convergent_block),
            );
        }

        self.switch_meta.get(&switch_block)
    }
}

pub(super) struct SwitchMeta {
    ignore: bool,
    targets_count: usize,
    common_locals: Rc<BTreeSet<Local>>,
    convergent_block: BasicBlock,
}

impl SwitchMeta {
    pub fn make(
        basic_blocks: &BasicBlocks<'_>,
        targets: &[BasicBlock],
        convergent_block: BasicBlock,
    ) -> Self {
        fn search_along_branch(
            basic_blocks: &BasicBlocks<'_>,
            start: BasicBlock,
            convergent_block: BasicBlock,
        ) -> FxHashSet<Local> {
            let mut locals: FxHashSet<Local> = Default::default();

            for block in BranchBlocks::new(basic_blocks, start, convergent_block) {
                let block_data = &basic_blocks[block];
                for stmt in &block_data.statements {
                    if let Some((place, _)) = stmt.kind.as_assign() {
                        locals.insert(place.local);
                    }
                }
                if let TerminatorKind::Call { destination, .. } =
                    block_data.terminator().kind
                {
                    locals.insert(destination.local);
                }
            }

            locals
        }

        let mut targets_count = 0;
        let mut targets = targets
            .iter()
            .filter(|target| {
                if basic_blocks[**target].is_empty_unreachable() {
                    return false;
                }

                targets_count += 1;
                **target != convergent_block
            })
            .copied();

        let mut common_locals =
            search_along_branch(basic_blocks, targets.next().unwrap(), convergent_block);
        let mut has_assign = !common_locals.is_empty();

        for target in targets {
            if basic_blocks[target].is_empty_unreachable() {
                continue;
            }

            let locals = search_along_branch(basic_blocks, target, convergent_block);

            common_locals = &common_locals & &locals;
            has_assign |= !locals.is_empty();
        }

        Self {
            ignore: !has_assign,
            targets_count,
            common_locals: Rc::new(common_locals.into_iter().collect()),
            convergent_block,
        }
    }
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

impl<'tcx> Compiler<'tcx> {
    #[instrument(level = "debug", skip(self, discr, targets, ctx, span), fields(fn_did = self.fn_name(ctx.fn_did)))]
    pub fn visit_switch(
        &mut self,
        switch_block: BasicBlock,
        discr: &Operand<'tcx>,
        targets: &SwitchTargets,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Option<BasicBlock>, Error> {
        let mir = ctx.mir;
        let basic_blocks = &ctx.mir.basic_blocks;
        let fn_did = ctx.fn_did;

        let discr = self.visit_operand(discr, ctx, span)?;
        let target_blocks = targets.all_targets();

        let switch_meta =
            match self.switch_meta(fn_did, switch_block, basic_blocks, target_blocks) {
                Some(switch_meta) => switch_meta,
                None => {
                    return Err(
                        SpanError::new(SpanErrorKind::NotSynthSwitch, span).into()
                    );
                }
            };

        let targets_count = switch_meta.targets_count;
        let convergent_block = switch_meta.convergent_block;

        if !switch_meta.ignore {
            if switch_meta.common_locals.is_empty() {
                error!("switchInt does not have common locals");
                return Err(SpanError::new(SpanErrorKind::NotSynthSwitch, span).into());
            }

            ctx.push_switch(Switch::new(
                switch_meta.common_locals.clone(),
                discr,
                targets_count,
            ));

            for (discr, target) in targets
                .iter()
                .map(|(discr, target)| (Some(discr), target))
                .chain(iter::once((None, targets.otherwise())))
            {
                if basic_blocks[target].is_empty_unreachable() {
                    continue;
                }

                if target != convergent_block {
                    let switch = ctx.last_switch().unwrap();
                    switch.add_branch(discr);

                    self.visit_blocks(Some(target), Some(convergent_block), ctx)?;
                } else {
                    let mut switch = ctx.pop_switch().unwrap();
                    switch.add_branch(discr);

                    for &local in switch.locals.clone().iter() {
                        let item = ctx.locals.get(local);
                        switch.add_branch_local(local, item);
                    }

                    ctx.push_switch(switch);
                }
            }

            let switch = ctx.pop_switch().unwrap();

            let (discr, discr_width) = match switch.discr.ty.kind() {
                ItemTyKind::Enum(enum_ty) => {
                    let discr_ty = enum_ty.discr_ty();
                    let discr = ctx.module.to_bitvec(&switch.discr);

                    (
                        Item::new(discr_ty, {
                            let splitter = SplitterArgs {
                                input: discr.port(),
                                outputs: iter::once((
                                    discr_ty.node_ty(),
                                    SymIdent::Discr.into(),
                                )),
                                start: None,
                                rev: true,
                            };
                            let splitter =
                                ctx.module.add_and_get_port::<_, Splitter>(splitter);

                            let span = self.span_to_string(span, ctx.fn_did);
                            ctx.module.add_span(splitter.node, span);

                            splitter
                        }),
                        enum_ty.discr_width(),
                    )
                }
                _ => (ctx.module.to_bitvec(&switch.discr), switch.discr.ty.width()),
            };
            let discr = discr.port();

            let output_ty = Ty::new_tup_from_iter(
                self.tcx,
                switch.locals.iter().map(|local| mir.local_decls[*local].ty),
            );
            let output_ty = self.resolve_ty(output_ty, ctx.generic_args, span)?;

            let default = switch.otherwise().map(|otherwise| {
                let item = Item::new(output_ty, Group::new(otherwise.values().cloned()));

                ctx.module.to_bitvec(&item).port()
            });

            let variants = IterMut::new(switch.branches(), |module, variant| {
                let case = ConstVal::new(variant.discr, discr_width);

                let item =
                    Item::new(output_ty, Group::new(variant.locals.values().cloned()));
                let input = module.to_bitvec(&item).port();

                (case, input)
            });

            let mux = MuxArgs {
                ty: output_ty.to_bitvec(),
                sel: discr,
                variants,
                default,
                sym: SymIdent::Mux.into(),
            };
            let mux = ctx.module.add_and_get_port::<_, Mux>(mux);

            let node_span = self.span_to_string(span, ctx.fn_did);
            ctx.module.add_span(mux.node, node_span);

            let mux = ctx.module.from_bitvec(mux, output_ty);

            for (local, item) in switch.locals.iter().zip(&*mux.group().items()) {
                self.assign_local(*local, item, ctx)?;
            }
        } else {
            debug!("ignore switchInt because it does have assigns");
        }

        Ok(Some(convergent_block))
    }

    fn switch_meta(
        &mut self,
        fn_did: DefId,
        switch_block: BasicBlock,
        basic_blocks: &BasicBlocks<'tcx>,
        targets: &[BasicBlock],
    ) -> Option<&SwitchMeta> {
        let switch_blocks = self
            .switch_meta
            .entry(fn_did)
            .or_insert_with(|| SwitchBlocks::make(basic_blocks));

        switch_blocks.switch_meta(switch_block, basic_blocks, targets)
    }
}
