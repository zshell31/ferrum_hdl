use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    iter,
    rc::Rc,
};

use either::Either;
use fhdl_netlist::{
    const_val::ConstVal,
    netlist::Port,
    node::{Merger, MergerArgs, Mux, MuxArgs},
};
use itertools::Itertools;
use petgraph::{
    algo::dominators::{simple_fast, Dominators},
    prelude::NodeIndex,
    stable_graph::IndexType,
    Directed, Graph,
};
use rustc_data_structures::{
    fx::{FxHashMap, FxHashSet, FxIndexSet},
    graph::WithSuccessors,
};
use rustc_middle::{
    mir::{
        BasicBlock, BasicBlocks, Local, Operand, Rvalue, SwitchTargets, TerminatorKind,
        START_BLOCK,
    },
    ty::Ty,
};
use rustc_span::Span;
use smallvec::{smallvec, SmallVec};
use tracing::{debug, debug_span, error, instrument};

use super::{
    item::{Group, Item, ModuleExt},
    Compiler, Context, SymIdent,
};
use crate::error::{Error, SpanError, SpanErrorKind};

#[derive(Debug, Clone)]
pub struct Branch<'tcx> {
    pub discr: ConstVal,
    pub locals: BTreeMap<Local, Item<'tcx>>,
}

impl<'tcx> Branch<'tcx> {
    fn new(discr: ConstVal) -> Self {
        Self {
            discr,
            locals: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Switch<'tcx> {
    pub locals: Rc<SwitchLocals>,
    branches: Vec<Branch<'tcx>>,
    is_otherwise: bool,
    otherwise: Option<BTreeMap<Local, Item<'tcx>>>,
}

impl<'tcx> Switch<'tcx> {
    pub(super) fn new(switch_meta: &SwitchMeta) -> Self {
        Self {
            locals: switch_meta.locals.clone(),
            branches: Vec::with_capacity(switch_meta.targets.len()),
            is_otherwise: false,
            otherwise: None,
        }
    }

    pub fn contains(&self, local: Local) -> bool {
        self.locals.has(local)
    }

    pub fn add_branch(&mut self, discr: Option<ConstVal>) {
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

    pub fn branches(&self) -> impl DoubleEndedIterator<Item = &Branch<'tcx>> {
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

pub(super) struct SwitchBlocks<'tcx> {
    post_dominators: Dominators<NodeIndex<BasicBlockWrap>>,
    switch_meta: FxHashMap<BasicBlock, Rc<SwitchMeta<'tcx>>>,
}

impl<'tcx> SwitchBlocks<'tcx> {
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

type Targets = Vec<(Option<SmallVec<[u128; 1]>>, BasicBlock)>;

#[derive(Debug)]
pub(super) struct SwitchMeta<'tcx> {
    ignore: bool,
    discr: Rc<SmallVec<[Operand<'tcx>; 1]>>,
    targets: Targets,
    locals: Rc<SwitchLocals>,
    pub(super) convergent_block: BasicBlock,
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

type SwitchTargetIter<'a, 'tcx: 'a> =
    impl Iterator<Item = (Option<u128>, BasicBlock)> + 'a;

trait SwitchTargetsExt {
    fn all_targets_with_def<'a, 'tcx>(
        &'a self,
        basic_blocks: &'a BasicBlocks<'tcx>,
    ) -> SwitchTargetIter<'a, 'tcx>
    where
        'tcx: 'a;
}

impl SwitchTargetsExt for SwitchTargets {
    fn all_targets_with_def<'a, 'tcx>(
        &'a self,
        basic_blocks: &'a BasicBlocks<'tcx>,
    ) -> SwitchTargetIter<'a, 'tcx>
    where
        'tcx: 'a,
    {
        self.iter()
            .map(|(val, target)| (Some(val), target))
            .chain(iter::once((None, self.otherwise())))
            .filter(|(_, target)| !basic_blocks[*target].is_empty_unreachable())
    }
}

type TargetIter<'a, 'tcx: 'a> = impl Iterator<Item = (Option<ConstVal>, BasicBlock)> + 'a;

impl<'tcx> Compiler<'tcx> {
    fn get_discr(
        &mut self,
        switch_meta: &SwitchMeta<'tcx>,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<SmallVec<[Port; 1]>, Error> {
        switch_meta
            .discr
            .iter()
            .map(|discr| {
                let discr = self.visit_operand(discr, ctx, span)?;
                Ok(ctx.module.get_discr(&discr))
            })
            .collect()
    }

    fn get_targets<'a>(
        &mut self,
        switch_meta: &'a SwitchMeta<'tcx>,
        discr: &'a [Port],
        ctx: &'a Context<'tcx>,
    ) -> Result<TargetIter<'a, 'tcx>, Error> {
        Ok(switch_meta
            .targets
            .iter()
            .map(move |(val, target)| match val {
                Some(val) => {
                    let val = (if val.len() == 1 {
                        Either::Left(iter::once(val[0]).chain(iter::repeat(0)))
                    } else {
                        assert_eq!(val.len(), discr.len());

                        Either::Right(val.iter().copied())
                    })
                    .zip(discr)
                    .fold(
                        ConstVal::zero(0),
                        |mut acc, (val, discr)| {
                            acc.shift(ConstVal::new(val, ctx.module[*discr].width()));
                            acc
                        },
                    );

                    (Some(val), *target)
                }
                None => (None, *target),
            }))
    }

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
        let discr_item = self.visit_operand(discr, ctx, span)?;

        let switch_meta = match self.switch_meta(
            switch_block,
            basic_blocks,
            discr.clone(),
            targets,
            &ctx.locals.switch_locals(),
            ctx,
        )? {
            Some(switch_meta) => switch_meta,
            None => {
                return Err(SpanError::new(SpanErrorKind::NotSynthSwitch, span).into());
            }
        };

        if discr_item.is_unsigned() && !has_projections {
            // handle `match unsigned<N>` block
            // case 0 responds to Unsigned::Short
            return Ok(Some(targets.target_for_value(0)));
        }

        let convergent_block = switch_meta.convergent_block;

        if !switch_meta.ignore {
            if switch_meta.locals.is_empty() {
                error!("switchInt does not have common locals");
                return Err(SpanError::new(SpanErrorKind::NotSynthSwitch, span).into());
            }

            let mux = match ctx.is_visited(switch_block) {
                Some(mux) => mux,
                None => {
                    let discr = self.get_discr(&switch_meta, ctx, span)?;
                    let targets = self
                        .get_targets(&switch_meta, &discr, ctx)?
                        .collect::<Vec<_>>();

                    ctx.push_switch(Switch::new(&switch_meta));
                    for (val, target) in targets {
                        if target != convergent_block {
                            let switch = ctx.last_switch().unwrap();
                            switch.add_branch(val);

                            self.visit_blocks(Some(target), Some(convergent_block), ctx)?;
                        } else {
                            let mut switch = ctx.pop_switch().unwrap();
                            switch.add_branch(val);
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

                    let output_ty = Ty::new_tup_from_iter(
                        self.tcx,
                        switch
                            .locals
                            .sorted()
                            .map(|local| mir.local_decls[local].ty),
                    );
                    let output_ty = self.resolve_ty(output_ty, ctx.generic_args, span)?;

                    let default = switch.otherwise().map(|otherwise| {
                        let item =
                            Item::new(output_ty, Group::new(otherwise.values().cloned()));
                        assert_eq!(output_ty.nodes(), item.nodes());
                        item.iter()
                    });

                    let variants = switch.branches().map(|variant| {
                        let item = Item::new(
                            output_ty,
                            Group::new(variant.locals.values().cloned()),
                        );
                        assert_eq!(output_ty.nodes(), item.nodes());
                        (variant.discr, item.iter())
                    });

                    let discr = if discr.len() == 1 {
                        discr[0]
                    } else {
                        ctx.module.add_and_get_port::<_, Merger>(MergerArgs {
                            inputs: discr,
                            rev: false,
                            sym: SymIdent::Sel.into(),
                        })
                    };

                    let mux = ctx.module.add::<_, Mux>(MuxArgs {
                        outputs: output_ty.iter().map(|ty| (ty, None)),
                        sel: discr,
                        variants,
                        default,
                    });
                    let node_span = self
                        .span_to_string(span, ctx.fn_did)
                        .map(|span| format!("{span} ({switch_block:?})"));
                    ctx.module.add_span(mux, node_span);

                    let mux = ctx.module.combine_from_node(mux, output_ty);

                    ctx.module
                        .assign_names_to_item(SymIdent::Mux.as_str(), &mux, false);

                    ctx.mark_as_visited(switch_block, &mux);

                    mux
                }
            };

            for (local, item) in switch_meta.locals.sorted().zip(&*mux.group().items()) {
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
        discr: Operand<'tcx>,
        targets: &SwitchTargets,
        outer: &SwitchLocals,
        ctx: &mut Context<'tcx>,
    ) -> Result<Option<Rc<SwitchMeta<'tcx>>>, Error> {
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
                    discr,
                    targets,
                    convergent_block,
                    outer,
                    ctx,
                )?;

                // debug!("{meta:#?}");

                meta
            };

            self.switch_meta
                .get_mut(&ctx.fn_did)
                .unwrap()
                .switch_meta
                .insert(switch_block, Rc::new(meta));
        }

        Ok(self.get_switch_meta(switch_block, ctx))
    }

    pub fn get_switch_meta(
        &self,
        switch_block: BasicBlock,
        ctx: &Context<'tcx>,
    ) -> Option<Rc<SwitchMeta<'tcx>>> {
        self.switch_meta
            .get(&ctx.fn_did)
            .unwrap()
            .switch_meta
            .get(&switch_block)
            .cloned()
    }

    fn make_switch_meta(
        &mut self,
        basic_blocks: &BasicBlocks<'tcx>,
        discr1: Operand<'tcx>,
        targets: &SwitchTargets,
        convergent_block: BasicBlock,
        outer: &SwitchLocals,
        ctx: &mut Context<'tcx>,
    ) -> Result<SwitchMeta<'tcx>, Error> {
        let otherwise1 = targets.otherwise();

        let mut common_discr = None;
        let mut nested_switches: FxIndexSet<BasicBlock> = Default::default();

        let mut search_along_branch = |start: BasicBlock| -> Result<SwitchLocals, Error> {
            let mut locals = SwitchLocals::default();
            let is_single_switch = self.is_single_switch(start, basic_blocks);

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

                let terminator = block_data.terminator();
                match &terminator.kind {
                    TerminatorKind::Call { destination, .. } => {
                        locals.add(destination.local, outer);
                        to_visit.extend(terminator.successors());
                    }
                    TerminatorKind::SwitchInt {
                        discr: discr2,
                        targets,
                    } => {
                        if let Some(switch_meta) = self.switch_meta(
                            block,
                            basic_blocks,
                            discr2.clone(),
                            targets,
                            // locals of the current branch + outer locals of the current switchInt
                            // block are outer to the next switchInt block
                            &SwitchLocals::from_outer(outer.outer().chain(locals.iter())),
                            ctx,
                        )? {
                            let can_merge_switches = self.can_merge_switches(
                                &discr1,
                                otherwise1,
                                &switch_meta.discr,
                                targets.otherwise(),
                                ctx,
                            );

                            if is_single_switch && can_merge_switches {
                                let discr = switch_meta.discr.clone();
                                if *common_discr.get_or_insert(discr) == switch_meta.discr
                                {
                                    nested_switches.insert(start);
                                }
                            }

                            locals.extend(&switch_meta.locals, outer);
                            to_visit.push_back(switch_meta.convergent_block);
                        }
                    }
                    _ => {
                        to_visit.extend(terminator.successors());
                    }
                }
            }

            Ok(locals)
        };

        let mut targets_iter = targets.all_targets_with_def(basic_blocks);

        let (_, target) = targets_iter.next().unwrap();
        let mut locals = search_along_branch(target)?;
        for (_, target) in targets_iter {
            locals.merge(search_along_branch(target)?);
        }

        let mut new_targets = Vec::with_capacity(targets.all_targets().len());
        let mut discr = smallvec![discr1];

        if common_discr.is_some() && !nested_switches.is_empty() {
            let common_discr = common_discr.unwrap();
            discr.extend(common_discr.iter().cloned());

            for (val1, target1) in targets.all_targets_with_def(basic_blocks) {
                if nested_switches.contains(&target1) {
                    let switch_meta = self.get_switch_meta(target1, ctx).unwrap();

                    new_targets.extend(
                        switch_meta
                            .targets
                            .iter()
                            .filter(|(val2, _)| val2.is_some())
                            .map(|(val2, target2)| {
                                let val = if let (Some(val1), Some(val2)) =
                                    (val1, val2.as_ref())
                                {
                                    let mut val1 = smallvec![val1];
                                    val1.extend(val2.iter().copied());
                                    Some(val1)
                                } else {
                                    None
                                };

                                (val, *target2)
                            }),
                    );
                } else {
                    new_targets.push((val1.map(|val| smallvec![val]), target1));
                }
            }
        } else {
            new_targets.extend(
                targets
                    .all_targets_with_def(basic_blocks)
                    .map(|(val, target)| (val.map(|val| smallvec![val]), target)),
            );
        }

        Ok(SwitchMeta {
            ignore: locals.ignore(),
            discr: Rc::new(discr),
            targets: new_targets,
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

    fn is_single_switch<'a>(
        &self,
        target: BasicBlock,
        basic_blocks: &'a BasicBlocks<'tcx>,
    ) -> bool {
        let block = &basic_blocks[target];

        block
            .statements
            .iter()
            .map(|stmt| {
                stmt.kind.as_assign().and_then(|(_, rvalue)| match rvalue {
                    Rvalue::Discriminant(discr) => Some(discr),
                    _ => None,
                })
            })
            .at_most_one()
            .ok()
            .is_some()
            && block.terminator().kind.as_switch().is_some()
    }

    fn can_merge_switches(
        &self,
        discr1: &Operand<'tcx>,
        otherwise1: BasicBlock,
        discr2: &[Operand<'tcx>],
        otherwise2: BasicBlock,
        ctx: &Context<'tcx>,
    ) -> bool {
        if otherwise1 == otherwise2 {
            let discr1_ty = discr1.place().map(|place| place.ty(ctx.mir, self.tcx).ty);

            discr2.iter().all(|discr2| {
                let discr2_ty =
                    discr2.place().map(|place| place.ty(ctx.mir, self.tcx).ty);
                discr1_ty == discr2_ty
            })
        } else {
            false
        }
    }
}
