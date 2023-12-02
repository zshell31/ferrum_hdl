use ferrum_hdl::const_functions::clog2;
use fhdl_netlist::{
    bvm::BitVecMask,
    net_list::{ModuleId, NodeOutId},
    node::{Case, NodeKind, Splitter, ZeroExtend},
    resolver::{Resolve, Resolver},
    sig_ty::{NodeTy, SignalTy, Width},
};
use rustc_macros::{Decodable, Encodable};

use super::Generator;

#[derive(Debug, Encodable, Decodable)]
pub enum TemplateNodeKind {
    TruncOrExtend(TruncOrExtend),
    CaseIndex(CaseIndex),
}

impl<R: Resolver> Resolve<R> for TemplateNodeKind {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(match self {
            Self::TruncOrExtend(kind) => {
                let kind = kind.resolve(resolver)?;
                Self::TruncOrExtend(kind)
            }
            Self::CaseIndex(kind) => {
                let kind = kind.resolve(resolver)?;
                Self::CaseIndex(kind)
            }
        })
    }
}

impl TemplateNodeKind {
    pub fn eval(self, module_id: ModuleId, generator: &mut Generator<'_>) -> NodeKind {
        match self {
            Self::TruncOrExtend(kind) => kind.eval(module_id, generator),
            Self::CaseIndex(kind) => kind.eval(module_id, generator),
        }
    }
}

trait EvalTemplateNodeKind: Sized + Into<TemplateNodeKind> {
    fn out_ty(&self) -> NodeTy;

    fn can_be_evaluated(&self, generator: &mut Generator<'_>) -> bool;

    fn eval(self, module_id: ModuleId, generator: &mut Generator<'_>) -> NodeKind;

    fn instantiate(
        self,
        module_id: ModuleId,
        generator: &mut Generator<'_>,
    ) -> NodeOutId {
        if self.can_be_evaluated(generator) {
            let node = self.eval(module_id, generator);

            generator.netlist.add_and_get_out(module_id, node)
        } else {
            let out_ty = self.out_ty();
            let node_id = generator.add_temp_node(module_id, self.into(), out_ty);

            generator.netlist[node_id].only_one_out().node_out_id()
        }
    }
}

#[derive(Debug, Encodable, Decodable)]
pub struct TruncOrExtend {
    from: NodeOutId,
    to_ty: NodeTy,
}

impl<R: Resolver> Resolve<R> for TruncOrExtend {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        let Self { from, to_ty } = self;
        let from = from.resolve(resolver)?;
        let to_ty = to_ty.resolve(resolver)?;

        Ok(Self { from, to_ty })
    }
}

impl From<TruncOrExtend> for TemplateNodeKind {
    fn from(kind: TruncOrExtend) -> Self {
        Self::TruncOrExtend(kind)
    }
}

impl EvalTemplateNodeKind for TruncOrExtend {
    fn out_ty(&self) -> NodeTy {
        self.to_ty
    }

    fn can_be_evaluated(&self, generator: &mut Generator<'_>) -> bool {
        let Self { from, to_ty } = self;
        let from_ty = generator.item_ty((*from).into());

        from_ty.width().is_value() && to_ty.width().is_value()
    }

    fn eval(self, _module_id: ModuleId, generator: &mut Generator<'_>) -> NodeKind {
        let Self { from, to_ty } = self;
        let from_ty = generator.item_ty(from.into());
        let from_width = from_ty.width().value();
        let to_width = to_ty.width().value();

        if from_width >= to_width {
            Splitter::new(from, [(to_ty, None)], None, false).into()
        } else {
            ZeroExtend::new(to_ty, from, None).into()
        }
    }
}

#[derive(Debug, Encodable, Decodable)]
pub struct CaseIndex {
    idx: NodeOutId,
    expr: NodeOutId,
    count: Width,
    item_ty: SignalTy,
}

impl<R> Resolve<R> for CaseIndex
where
    R: Resolver,
{
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        let Self {
            idx,
            expr,
            count,
            item_ty,
        } = self;

        let idx = idx.resolve(resolver)?;
        let expr = expr.resolve(resolver)?;
        let count = count.resolve(resolver)?;
        let item_ty = item_ty.resolve(resolver)?;

        Ok(Self {
            idx,
            expr,
            count,
            item_ty,
        })
    }
}

impl From<CaseIndex> for TemplateNodeKind {
    fn from(kind: CaseIndex) -> Self {
        TemplateNodeKind::CaseIndex(kind)
    }
}

impl EvalTemplateNodeKind for CaseIndex {
    fn out_ty(&self) -> NodeTy {
        NodeTy::BitVec(self.item_ty.width())
    }

    fn can_be_evaluated(&self, _generator: &mut Generator<'_>) -> bool {
        let width = self.item_ty.width();

        self.count.is_value() && width.is_value()
    }

    fn eval(self, module_id: ModuleId, generator: &mut Generator<'_>) -> NodeKind {
        let width = self.item_ty.width().value();
        assert!(width != 0);

        let count = self.count.value();
        let variant_ty = self.out_ty();

        struct StepBy {
            current: u128,
            step: u128,
        }

        impl Iterator for StepBy {
            type Item = u128;

            fn next(&mut self) -> Option<Self::Item> {
                let res = self.current;
                self.current += self.step;
                Some(res)
            }
        }

        let variant_width = clog2(count as usize) as u128;

        let default = {
            let mut mask = BitVecMask::default();
            mask.set_val(0, variant_width);

            generator.netlist.add_and_get_out(
                module_id,
                Splitter::new(self.expr, [(variant_ty, None)], Some(0.into()), false),
            )
        };
        let variants = StepBy {
            current: 0,
            step: width,
        }
        .take(count as usize)
        .enumerate()
        .map(|(num, idx)| {
            let mut mask = BitVecMask::default();
            mask.set_val(num as u128, variant_width);

            let variant = generator.netlist.add_and_get_out(
                module_id,
                Splitter::new(self.expr, [(variant_ty, None)], Some(idx.into()), false),
            );

            (mask, variant)
        });

        Case::new(variant_ty, self.idx, variants, Some(default), None).into()
    }
}

impl<'tcx> Generator<'tcx> {
    pub fn trunc_or_extend(
        &mut self,
        module_id: ModuleId,
        from: NodeOutId,
        to_ty: NodeTy,
    ) -> NodeOutId {
        TruncOrExtend { from, to_ty }.instantiate(module_id, self)
    }

    pub fn case_index(
        &mut self,
        module_id: ModuleId,
        idx: NodeOutId,
        expr: NodeOutId,
        count: Width,
        item_ty: SignalTy,
    ) -> NodeOutId {
        CaseIndex {
            idx,
            expr,
            count,
            item_ty,
        }
        .instantiate(module_id, self)
    }
}
