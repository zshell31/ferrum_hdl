use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    iter,
    rc::Rc,
};

use fhdl_netlist::{
    const_val::ConstVal,
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
use rustc_middle::{
    mir::{
        BasicBlock, BasicBlocks, Local, Operand, SwitchTargets, TerminatorKind,
        START_BLOCK,
    },
    ty::Ty,
};
use rustc_span::Span;
use tracing::{debug, debug_span, error, instrument};

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
    pub locals: Rc<SwitchLocals>,
    pub discr: Item<'tcx>,
    targets: usize,
    branches: Vec<Branch<'tcx>>,
    is_otherwise: bool,
    otherwise: Option<BTreeMap<Local, Item<'tcx>>>,
}

impl<'tcx> Switch<'tcx> {
    pub fn new(locals: Rc<SwitchLocals>, discr: Item<'tcx>, targets: usize) -> Self {
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
        self.locals.has(local)
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
        assert!(self.locals.has(local));
        if self.is_otherwise {
            self.otherwise.as_mut().unwrap().insert(local, item);
        } else {
            self.branches.last_mut().unwrap().locals.insert(local, item);
        }
    }

    pub fn branch_local(&self, local: Local) -> Option<&Item<'tcx>> {
        assert!(self.locals.has(local));
        if self.is_otherwise {
            self.otherwise.as_ref().unwrap().get(&local)
        } else {
            self.branches.last().unwrap().locals.get(&local)
        }
    }

    pub fn has_branch_local(&self, local: Local) -> bool {
        if self.is_otherwise {
            self.otherwise.as_ref().unwrap().contains_key(&local)
        } else {
            self.branches.last().unwrap().locals.contains_key(&local)
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
                assert!(branch.locals.contains_key(&local));
            }

            branch
        })
    }

    pub fn otherwise(&self) -> Option<&BTreeMap<Local, Item<'tcx>>> {
        let otherwise = self.otherwise.as_ref()?;
        assert_eq!(self.locals.len(), otherwise.len());
        for local in self.locals.iter() {
            assert!(otherwise.contains_key(&local));
        }

        self.otherwise.as_ref()
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
}

#[derive(Debug, Default)]
pub struct SwitchLocals {
    // Locals common for all branches of the current switchInt block
    common: FxHashSet<Local>,
    // Locals outer to the current switchInt block
    outer: FxHashSet<Local>,
    // Is there an assignment for any of the common or outer locals in at least one branch
    has_assign: bool,
}

impl SwitchLocals {
    pub fn from_outer(outer: impl Iterator<Item = Local>) -> Self {
        Self {
            outer: outer.collect(),
            ..Default::default()
        }
    }

    pub fn add(&mut self, local: Local, outer: &Self) {
        if outer.has(local) {
            self.outer.insert(local);
        } else {
            self.common.insert(local);
        }

        self.has_assign = true;
    }

    pub fn has(&self, local: Local) -> bool {
        self.common.contains(&local) || self.outer.contains(&local)
    }

    pub fn extend(&mut self, locals: &Self, outer: &Self) {
        self.common.extend(locals.common.iter().copied());
        self.outer.extend(
            locals
                .outer
                .iter()
                .copied()
                .filter(|local| outer.has(*local)),
        );
        self.has_assign |= locals.has_assign;
    }

    pub fn ignore(&self) -> bool {
        !self.has_assign
    }

    pub fn merge(&mut self, locals: Self) {
        self.has_assign |= locals.has_assign;
        self.common = &self.common & &locals.common;
        self.outer = &self.outer | &locals.outer;
    }

    pub fn is_empty(&self) -> bool {
        self.common.is_empty() && self.outer.is_empty()
    }

    pub fn len(&self) -> usize {
        self.common.len() + self.outer.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = Local> + '_ {
        self.common.iter().chain(self.outer.iter()).copied()
    }

    pub fn outer(&self) -> impl Iterator<Item = Local> + '_ {
        self.outer.iter().copied()
    }

    pub fn sorted(&self) -> impl Iterator<Item = Local> {
        let set: BTreeSet<Local> = self.iter().collect();
        set.into_iter()
    }
}

#[derive(Debug)]
pub(super) struct SwitchMeta {
    ignore: bool,
    targets_count: usize,
    locals: Rc<SwitchLocals>,
    convergent_block: BasicBlock,
}

impl SwitchMeta {}

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
    #[instrument(level = "debug", skip(self, discr, targets, ctx, span))]
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

        let has_projections = self.has_projections(discr);
        let discr = self.visit_operand(discr, ctx, span)?;

        let switch_meta = match self.switch_meta(
            switch_block,
            basic_blocks,
            targets,
            &ctx.locals.switch_locals(),
            ctx,
        )? {
            Some(switch_meta) => switch_meta,
            None => {
                return Err(SpanError::new(SpanErrorKind::NotSynthSwitch, span).into());
            }
        };

        if discr.is_unsigned() && !has_projections {
            // handle `match unsigned<N>` block
            // case 0 responds to Unsigned::Short
            return Ok(Some(targets.target_for_value(0)));
        }

        let targets_count = switch_meta.targets_count;
        let convergent_block = switch_meta.convergent_block;

        if !switch_meta.ignore {
            if switch_meta.locals.is_empty() {
                error!("switchInt does not have common locals");
                return Err(SpanError::new(SpanErrorKind::NotSynthSwitch, span).into());
            }

            ctx.push_switch(Switch::new(
                switch_meta.locals.clone(),
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
                    ctx.push_switch(switch);
                }

                // Share outer locals to all branches that do not contain assignments for them.
                // For example
                // ```rust
                // let mut k = 0;
                // let mut n = 1;
                // if v {
                //     k = 1;
                // } else {
                //     n = 0;
                // }
                // ````
                //
                // As result:
                // - if-branch will get `n = 1`
                // - else-branch will get `k = 0`
                let mut switch = ctx.pop_switch().unwrap();
                for local in switch.locals.clone().outer() {
                    if !switch.has_branch_local(local) {
                        let item = ctx.locals.get(local);
                        switch.add_branch_local(local, item);
                    }
                }
                ctx.push_switch(switch);
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
                switch
                    .locals
                    .sorted()
                    .map(|local| mir.local_decls[local].ty),
            );
            let output_ty = self.resolve_ty(output_ty, ctx.generic_args, span)?;

            let default = switch.otherwise().map(|otherwise| {
                let item = Item::new(output_ty, Group::new(otherwise.values().cloned()));
                item.iter()
            });

            let variants = switch.branches().map(|variant| {
                let case = ConstVal::new(variant.discr, discr_width);
                let item =
                    Item::new(output_ty, Group::new(variant.locals.values().cloned()));
                (case, item.iter())
            });

            let mux = MuxArgs {
                outputs: output_ty.iter().map(|ty| (ty, None)),
                sel: discr,
                variants,
                default,
            };
            let mux = ctx.module.add::<_, Mux>(mux);
            let node_span = self.span_to_string(span, ctx.fn_did);
            ctx.module.add_span(mux, node_span);

            let mux = ctx.module.combine_from_node(mux, output_ty);
            ctx.module
                .assign_names_to_item(SymIdent::Mux.as_str(), &mux, false);

            for (local, item) in switch.locals.sorted().zip(&*mux.group().items()) {
                self.assign_local(local, item, ctx)?;
            }
        } else {
            debug!("ignore switchInt because it does have assigns");
        }

        Ok(Some(convergent_block))
    }

    fn switch_meta(
        &mut self,
        switch_block: BasicBlock,
        basic_blocks: &BasicBlocks<'tcx>,
        targets: &SwitchTargets,
        outer: &SwitchLocals,
        ctx: &mut Context<'tcx>,
    ) -> Result<Option<&SwitchMeta>, Error> {
        let switch_blocks = self
            .switch_meta
            .entry(ctx.fn_did)
            .or_insert_with(|| SwitchBlocks::make(basic_blocks));

        #[allow(clippy::map_entry)]
        if !switch_blocks.switch_meta.contains_key(&switch_block) {
            let convergent_block =
                match switch_blocks.immediate_post_dominator(switch_block) {
                    Some(convergent_block) => convergent_block,
                    None => {
                        error!("Cannot find convergent block for switchInt targets");
                        return Ok(None);
                    }
                };

            let span = debug_span!("switch_meta", switch_block = ?switch_block);
            let meta = {
                let _enter = span.enter();
                let meta = self.make_switch_meta(
                    basic_blocks,
                    targets,
                    convergent_block,
                    outer,
                    ctx,
                )?;

                debug!("{meta:#?}");

                meta
            };

            self.switch_meta
                .get_mut(&ctx.fn_did)
                .unwrap()
                .switch_meta
                .insert(switch_block, meta);
        }

        Ok(self
            .switch_meta
            .get(&ctx.fn_did)
            .unwrap()
            .switch_meta
            .get(&switch_block))
    }

    fn make_switch_meta(
        &mut self,
        basic_blocks: &BasicBlocks<'tcx>,
        targets: &SwitchTargets,
        convergent_block: BasicBlock,
        outer: &SwitchLocals,
        ctx: &mut Context<'tcx>,
    ) -> Result<SwitchMeta, Error> {
        let targets = targets.all_targets();

        let mut search_along_branch = |start: BasicBlock| -> Result<SwitchLocals, Error> {
            let mut locals = SwitchLocals::default();
            let mut to_visit = VecDeque::default();

            to_visit.push_back(start);
            while let Some(block) = to_visit.pop_front() {
                if block == convergent_block {
                    break;
                }

                let block_data = &basic_blocks[block];
                if block_data.is_empty_unreachable() {
                    to_visit.extend(block_data.terminator().successors());
                    continue;
                }

                for stmt in &block_data.statements {
                    if let Some((place, _)) = stmt.kind.as_assign() {
                        locals.add(place.local, outer);
                    }
                }

                let mut is_switch_block = false;
                let terminator = block_data.terminator();
                match &terminator.kind {
                    TerminatorKind::Call { destination, .. } => {
                        locals.add(destination.local, outer);
                    }
                    TerminatorKind::SwitchInt { targets, .. } => {
                        if let Some(switch_meta) = self.switch_meta(
                            block,
                            basic_blocks,
                            targets,
                            // locals of the current branch + outer locals of the current switchInt
                            // block are outer to the next switchInt block
                            &SwitchLocals::from_outer(outer.outer().chain(locals.iter())),
                            ctx,
                        )? {
                            locals.extend(&switch_meta.locals, outer);

                            is_switch_block = true;
                            to_visit.push_back(switch_meta.convergent_block);
                        }
                    }
                    _ => {}
                }

                if !is_switch_block {
                    to_visit.extend(block_data.terminator().successors());
                }
            }

            Ok(locals)
        };

        let mut targets_count = 0;
        let mut targets = targets
            .iter()
            .filter(|target| {
                if basic_blocks[**target].is_empty_unreachable() {
                    false
                } else {
                    targets_count += 1;
                    true
                }
            })
            .copied();

        let target = targets.next().unwrap();
        let mut locals = search_along_branch(target)?;
        for target in targets {
            if basic_blocks[target].is_empty_unreachable() {
                continue;
            }

            locals.merge(search_along_branch(target)?);
        }

        Ok(SwitchMeta {
            ignore: locals.ignore(),
            targets_count,
            locals: Rc::new(locals),
            convergent_block,
        })
    }

    fn has_projections(&self, operand: &Operand<'tcx>) -> bool {
        operand
            .place()
            .filter(|place| !place.projection.is_empty())
            .is_some()
    }
}
