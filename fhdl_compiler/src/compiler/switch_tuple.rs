use std::{fmt::Debug, rc::Rc};

use fhdl_netlist::{
    const_val::ConstVal,
    node::{Case, TupleCase},
};
use rustc_data_structures::fx::FxIndexMap;
use rustc_middle::{
    mir::{BasicBlock, BasicBlocks, Local, Operand, Place, PlaceElem, TerminatorKind},
    ty::{List, TyKind},
};
use rustc_span::Span;

use super::{item_ty::ItemTy, switch::SwitchTargetsExt, Compiler, Context};
use crate::error::Error;

pub trait BasicBlocksExt {
    fn is_switch(&self, block: BasicBlock) -> bool;
}

impl<'tcx> BasicBlocksExt for BasicBlocks<'tcx> {
    fn is_switch(&self, block: BasicBlock) -> bool {
        let bbs = &self[block];
        matches!(bbs.terminator().kind, TerminatorKind::SwitchInt { .. })
    }
}

#[derive(Debug)]
pub struct SwitchTuple<'tcx> {
    discr_tuple: Local,
    discr_tuple_ty: ItemTy<'tcx>,
    cases: FxIndexMap<TupleCase, BasicBlock>,
    otherwise: BasicBlock,
}

pub type SwitchTupleRef<'tcx> = Rc<SwitchTuple<'tcx>>;

impl<'tcx> SwitchTuple<'tcx> {
    fn new(
        discr_tuple: Local,
        discr_tuple_ty: ItemTy<'tcx>,
        otherwise: BasicBlock,
    ) -> Self {
        Self {
            discr_tuple,
            discr_tuple_ty,
            cases: Default::default(),
            otherwise,
        }
    }

    fn push_val(&self, cases: &mut TupleCase, discr: &Place<'tcx>, val2: u128) -> bool {
        let idx = match discr.projection[0] {
            PlaceElem::Field(idx, _) => idx.as_usize(),
            _ => panic!("expected tuple"),
        };
        let ty = self.discr_tuple_ty.struct_ty().by_idx(idx);
        let val2 = ConstVal::new(val2, ty.width());

        match cases.0[idx] {
            Case::Val(val1) => {
                if val1 != val2 {
                    return false;
                }
            }
            Case::Default(_) => {
                cases.0[idx] = Case::Val(val2);
            }
        }

        true
    }

    fn add_tuple_case(&mut self, new_case: TupleCase, new_block: BasicBlock) {
        let mut add = true;
        self.cases.retain(|case, block| {
            if new_case.include(case) && *block == new_block {
                false
            } else {
                if case.include(&new_case) && *block == new_block {
                    add = false;
                }
                true
            }
        });

        if add {
            self.cases.insert(new_case, new_block);
        }
    }

    #[inline]
    pub fn discr_tuple(&self) -> Place<'tcx> {
        Place {
            local: self.discr_tuple,
            projection: List::empty(),
        }
    }
}

impl<'tcx> SwitchTargetsExt for SwitchTuple<'tcx> {
    type Value = TupleCase;

    fn variants(&self) -> impl Iterator<Item = (usize, BasicBlock)> {
        self.cases.values().copied().enumerate()
    }

    fn otherwise(&self) -> BasicBlock {
        self.otherwise
    }

    fn value_for_target(&self, idx: usize, _: u128) -> Self::Value {
        self.cases.keys()[idx].clone()
    }
}

impl<'tcx> Compiler<'tcx> {
    pub fn is_switch_tuple(
        &mut self,
        block: BasicBlock,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Option<SwitchTupleRef<'tcx>>, Error> {
        let key = (ctx.fn_did, block);
        #[allow(clippy::map_entry)]
        if !self.switch_tuples.contains_key(&key) {
            let switch_tuple = self.try_make_switch_tuple(block, ctx, span)?;
            self.switch_tuples.insert(key, switch_tuple.map(Rc::new));
        }

        Ok(self.switch_tuples.get(&key).unwrap().clone())
    }

    fn try_make_switch_tuple(
        &mut self,
        block: BasicBlock,
        ctx: &mut Context<'tcx>,
        span: Span,
    ) -> Result<Option<SwitchTuple<'tcx>>, Error> {
        let otherwise = match self.find_otherwise(block, ctx) {
            Some(otherwise) => otherwise,
            None => {
                return Ok(None);
            }
        };

        let mut discr_tuple = None;
        if !self.collect_discr(&mut discr_tuple, block, ctx) || discr_tuple.is_none() {
            return Ok(None);
        }
        let discr_tuple = discr_tuple.unwrap();
        let discr_tuple_ty = ctx.mir.local_decls[discr_tuple].ty;
        let discr_tuple_ty = self.resolve_ty(discr_tuple_ty, ctx.generic_args, span)?;
        let mut switch = SwitchTuple::new(discr_tuple, discr_tuple_ty, otherwise);

        let case = self.init_default_case(discr_tuple_ty);
        self.collect_tuples(&mut switch, case, block, ctx);

        Ok(Some(switch))
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

    fn init_default_case(&mut self, discr_tuple_ty: ItemTy<'tcx>) -> TupleCase {
        let defaults = discr_tuple_ty
            .struct_ty()
            .tys()
            .map(|ty| Case::Default(ty.width()))
            .collect();

        TupleCase(defaults)
    }

    fn collect_discr(
        &self,
        discr_tuple: &mut Option<Local>,
        block: BasicBlock,
        ctx: &Context<'tcx>,
    ) -> bool {
        let blocks = &ctx.mir.basic_blocks;
        let bbs = &blocks[block];

        if let TerminatorKind::SwitchInt {
            discr: Operand::Copy(discr) | Operand::Move(discr),
            targets,
        } = &bbs.terminator().kind
        {
            if self.discr_has_inner_ty(bbs, ctx) {
                let target = targets.target_for_value(0);
                return self.collect_discr(discr_tuple, target, ctx);
            }

            match ctx.mir.local_decls[discr.local].ty.kind() {
                TyKind::Tuple(_) if !discr.projection.is_empty() => {
                    if *discr_tuple.get_or_insert(discr.local) != discr.local {
                        return false;
                    }
                }
                _ => {
                    return false;
                }
            }

            for &target in targets.all_targets() {
                if blocks.is_switch(target)
                    && !self.collect_discr(discr_tuple, target, ctx)
                {
                    return false;
                }
            }
        }

        true
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
}
