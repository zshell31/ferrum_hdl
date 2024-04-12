use std::{
    cmp::Ordering,
    collections::VecDeque,
    fmt::{self, Debug, Display},
    iter,
};

use itertools::Itertools;
use rustc_data_structures::{
    fx::{FxIndexMap, FxIndexSet},
    graph::WithSuccessors,
};
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, BasicBlocks, Operand, Place, PlaceElem, Rvalue,
    TerminatorKind, START_BLOCK,
};
use tracing::debug;

use super::{Compiler, Context};

pub trait BasicBlocksExt {
    fn is_switch(&self, block: BasicBlock) -> bool;
}

impl<'tcx> BasicBlocksExt for BasicBlocks<'tcx> {
    fn is_switch(&self, block: BasicBlock) -> bool {
        let bbs = &self[block];
        matches!(bbs.terminator().kind, TerminatorKind::SwitchInt { .. })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Case {
    Val(u128),
    Default,
}

impl From<Option<u128>> for Case {
    fn from(val: Option<u128>) -> Self {
        match val {
            Some(val) => Self::Val(val),
            None => Self::Default,
        }
    }
}

impl Debug for Case {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Val(val) => Display::fmt(val, f),
            Self::Default => f.write_str("_"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TupleCase {
    tuple: Vec<Case>,
    group: u8,
    non_default: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Inclusion {
    Eq,
    Include,
    IncludedBy,
    DontIntersect,
}

impl TupleCase {
    fn new(len: usize) -> Self {
        Self {
            tuple: iter::repeat(Case::Default).take(len).collect(),
            group: 0,
            non_default: 0,
        }
    }

    fn include(&self, other: &Self) -> Inclusion {
        let mut inclusion = Inclusion::Eq;

        for (this, other) in self.tuple.iter().zip(other.tuple.iter()) {
            inclusion = match (this, other) {
                (Case::Val(this), Case::Val(other)) => {
                    if this != other {
                        return Inclusion::DontIntersect;
                    } else {
                        inclusion
                    }
                }
                (Case::Default, Case::Val(_)) => Inclusion::Include,
                (Case::Val(_), Case::Default) => Inclusion::IncludedBy,
                (Case::Default, Case::Default) => inclusion,
            };
        }

        inclusion
    }
}

impl Debug for TupleCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[ ")?;
        let mut first = true;
        for case in &self.tuple {
            if !first {
                f.write_str(", ")?;
            } else {
                first = false;
            }

            Debug::fmt(case, f)?;
        }
        f.write_str(" ]")?;
        // write!(f, " ] ({}, {})", self.non_default, self.group)?;
        f.write_str(" ]")?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct SwitchTuple<'tcx> {
    discr: FxIndexSet<Place<'tcx>>,
    cases: FxIndexMap<TupleCase, BasicBlock>,
    otherwise: BasicBlock,
}

impl<'tcx> SwitchTuple<'tcx> {
    fn new(otherwise: BasicBlock) -> Self {
        Self {
            discr: Default::default(),
            cases: Default::default(),
            otherwise,
        }
    }

    fn sort_discr(&mut self) {
        // Try to sort places by local and fields (if there are)
        self.discr.sort_unstable_by(|a, b| -> Ordering {
            a.local
                .cmp(&b.local)
                .then_with(|| a.projection.len().cmp(&b.projection.len()))
                .then_with(|| {
                    for (a_proj, b_proj) in a.projection.iter().zip(b.projection.iter()) {
                        return match (a_proj, b_proj) {
                            (PlaceElem::Field(a_idx, _), PlaceElem::Field(b_idx, _)) => {
                                let cmp = a_idx.cmp(&b_idx);
                                if cmp == Ordering::Equal {
                                    continue;
                                } else {
                                    cmp
                                }
                            }
                            (PlaceElem::Field(_, _), _) => Ordering::Greater,
                            (_, PlaceElem::Field(_, _)) => Ordering::Less,
                            (_, _) => continue,
                        };
                    }

                    Ordering::Equal
                })
        });
    }

    fn init_case(&self) -> TupleCase {
        TupleCase::new(self.discr.len())
    }

    fn push_val(&self, cases: &mut TupleCase, discr: &Place<'tcx>, val2: u128) -> bool {
        let idx = self.discr.get_index_of(discr).unwrap();
        match cases.tuple[idx] {
            Case::Val(val1) => {
                if val1 != val2 {
                    return false;
                }
            }
            Case::Default => {
                cases.tuple[idx] = Case::Val(val2);
                cases.non_default += 1;
            }
        }

        true
    }

    #[inline]
    fn add_tuple_case(&mut self, new_case: TupleCase, new_block: BasicBlock) {
        if !self.cases.contains_key(&new_case) {
            self.cases.insert(new_case, new_block);
        }
    }
}

#[derive(Debug, Default)]
pub struct SwitchTuples<'tcx> {
    blocks: FxIndexMap<BasicBlock, SwitchTuple<'tcx>>,
}

impl<'tcx> SwitchTuples<'tcx> {
    fn insert(&mut self, block: BasicBlock, switch: SwitchTuple<'tcx>) {
        self.blocks.insert(block, switch);
    }
}

impl<'tcx> Compiler<'tcx> {
    pub fn collect_switch_tuples(&self, ctx: &Context<'tcx>) -> SwitchTuples<'tcx> {
        let mut switches = SwitchTuples::default();

        let mut to_visit = VecDeque::default();
        to_visit.push_back(START_BLOCK);

        let blocks = &ctx.mir.basic_blocks;
        while let Some(block) = to_visit.pop_front() {
            let bbs = &blocks[block];
            if let TerminatorKind::SwitchInt { targets, .. } = &bbs.terminator().kind {
                if self.discr_has_inner_ty(bbs, ctx) {
                    to_visit.push_back(targets.target_for_value(0));
                    continue;
                }

                if let Some(switch) = self.try_make_switch_tuple(block, ctx) {
                    switches.insert(block, switch);

                    let meta = self.get_switch_meta(block, ctx).unwrap();
                    to_visit.push_back(meta.convergent_block);
                    continue;
                }
            }

            to_visit.extend(blocks.successors(block));
        }

        switches
    }

    fn try_make_switch_tuple(
        &self,
        block: BasicBlock,
        ctx: &Context<'tcx>,
    ) -> Option<SwitchTuple<'tcx>> {
        let otherwise = self.find_otherwise(block, ctx)?;
        let mut switch = SwitchTuple::new(otherwise);

        self.collect_discr(&mut switch, block, ctx);
        if switch.discr.len() == 1 {
            return None;
        }

        switch.sort_discr();
        let case = switch.init_case();
        self.collect_tuples(&mut switch, case, block, ctx);

        Some(switch)
    }

    fn find_otherwise(
        &self,
        block: BasicBlock,
        ctx: &Context<'tcx>,
    ) -> Option<BasicBlock> {
        let blocks = &ctx.mir.basic_blocks;
        let bbs = &blocks[block];

        if let TerminatorKind::SwitchInt { targets, .. } = &bbs.terminator().kind {
            if self.discr_has_inner_ty(bbs, ctx) {
                self.find_otherwise(targets.target_for_value(0), ctx)
            } else {
                self.find_otherwise(targets.otherwise(), ctx)
            }
        } else if bbs.is_empty_unreachable() {
            None
        } else {
            Some(block)
        }
    }

    fn collect_discr(
        &self,
        switch: &mut SwitchTuple<'tcx>,
        block: BasicBlock,
        ctx: &Context<'tcx>,
    ) {
        let blocks = &ctx.mir.basic_blocks;
        let bbs = &blocks[block];

        if let TerminatorKind::SwitchInt {
            discr: Operand::Copy(discr) | Operand::Move(discr),
            targets,
        } = &bbs.terminator().kind
        {
            if self.discr_has_inner_ty(bbs, ctx) {
                let target = targets.target_for_value(0);
                return self.collect_discr(switch, target, ctx);
            }

            switch.discr.insert(*discr);

            for &target in targets.all_targets() {
                if blocks.is_switch(target) {
                    self.collect_discr(switch, target, ctx);
                }
            }
        }
    }

    fn collect_tuples(
        &self,
        switch: &mut SwitchTuple<'tcx>,
        case: TupleCase,
        block: BasicBlock,
        ctx: &Context<'tcx>,
    ) {
        let blocks = &ctx.mir.basic_blocks;
        let bbs = &blocks[block];

        if let TerminatorKind::SwitchInt {
            discr: Operand::Copy(discr) | Operand::Move(discr),
            targets,
        } = &bbs.terminator().kind
        {
            if self.discr_has_inner_ty(bbs, ctx) {
                return self.collect_tuples(
                    switch,
                    case,
                    targets.target_for_value(0),
                    ctx,
                );
            }

            // let otherwise = self.find_otherwise(block, ctx);
            // if otherwise != Some(switch.otherwise) {
            //     switch.add_tuple_case(case, block);
            //     return;
            // }

            for (val, target) in targets.iter() {
                let mut cases = case.clone();
                if switch.push_val(&mut cases, discr, val) {
                    self.collect_tuples(switch, cases, target, ctx);
                }
            }

            if targets.otherwise() != switch.otherwise {
                let case = case.clone();
                self.collect_tuples(switch, case, targets.otherwise(), ctx);
            }
        } else {
            switch.add_tuple_case(case, block);
        }
    }

    pub fn discr_has_inner_ty(
        &self,
        bbs: &BasicBlockData<'tcx>,
        ctx: &Context<'tcx>,
    ) -> bool {
        let discr = match bbs
            .statements
            .iter()
            .filter_map(|stmt| {
                stmt.kind.as_assign().and_then(|(_, rvalue)| match rvalue {
                    Rvalue::Discriminant(discr) => Some(*discr),
                    _ => None,
                })
            })
            .exactly_one()
        {
            Ok(discr) => discr,
            _ => {
                return false;
            }
        };

        let discr_ty = discr.ty(ctx.mir, self.tcx).ty;
        self.is_inner_ty(discr_ty)
    }
}
