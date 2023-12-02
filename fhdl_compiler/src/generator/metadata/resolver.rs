use std::collections::{hash_map::Entry, VecDeque};

use fhdl_netlist::{
    net_list::{Idx, ModuleId, ParamId, TyId},
    resolver::Resolver,
    sig_ty::NodeTy,
};
use rustc_hir::def_id::DefId;
use rustc_middle::ty::Ty;
use rustc_span::Span;

use super::{Generator, Metadata, ModuleKey};
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
};

#[derive(Clone, Copy)]
pub struct ImportModule {
    pub source: ModuleId,
    pub target: ModuleId,
}

pub struct MetadataResolver<'g, 'tcx> {
    generator: &'g mut Generator<'tcx>,
    metadata: &'g Metadata<'tcx>,
    fn_did: DefId,
    modules_to_eval: &'g mut VecDeque<ImportModule>,
    ctx: &'g EvalContext<'tcx>,
    span: Span,
}

impl<'g, 'tcx> MetadataResolver<'g, 'tcx> {
    pub fn new(
        generator: &'g mut Generator<'tcx>,
        metadata: &'g Metadata<'tcx>,
        fn_did: DefId,
        modules_to_eval: &'g mut VecDeque<ImportModule>,
        ctx: &'g EvalContext<'tcx>,
        span: Span,
    ) -> Self {
        Self {
            generator,
            metadata,
            fn_did,
            modules_to_eval,
            ctx,
            span,
        }
    }

    fn find_ty(&self, ty_id: TyId) -> Result<Ty<'tcx>, Error> {
        self.metadata
            .types
            .get(ty_id.idx())
            .copied()
            .ok_or_else(|| {
                SpanError::new(
                    SpanErrorKind::MissingNodeTyForDef(
                        ty_id,
                        self.generator.tcx.def_path_str(self.fn_did),
                    ),
                    self.span,
                )
                .into()
            })
    }
}

impl<'g, 'tcx> Resolver for MetadataResolver<'g, 'tcx> {
    type Error = Error;

    fn resolve_node_ty(&mut self, ty_id: TyId) -> Result<NodeTy, Self::Error> {
        let ty = self.find_ty(ty_id)?;
        let sig_ty = self.generator.find_sig_ty(ty, self.ctx, self.span)?;

        sig_ty
            .opt_node_ty()
            .ok_or_else(|| {
                SpanError::new(
                    SpanErrorKind::ExpectedNodeTypeForDef(
                        ty.to_string(),
                        self.generator.tcx.def_path_str(self.fn_did),
                    ),
                    self.span,
                )
            })
            .map_err(Into::into)
    }

    fn resolve_const_param(
        &mut self,
        ty_id: TyId,
        param_id: ParamId,
    ) -> Result<u128, Self::Error> {
        let ty = self.find_ty(ty_id)?;
        let sig_ty_info = self.generator.find_sig_ty_info(ty, self.ctx, self.span)?;
        sig_ty_info
            .ty_or_def_id
            .generic_const_value(param_id.idx(), self.span)
    }

    fn resolve_module_id(&mut self, module_id: ModuleId) -> ModuleId {
        let krate = self.fn_did.krate;
        let key = ModuleKey { krate, module_id };

        if let Entry::Vacant(e) = self.generator.imported_modules.entry(key) {
            let source = &self.metadata.netlist()[module_id];
            let target = self.generator.netlist.add_module(source.name, false);

            self.modules_to_eval.push_back(ImportModule {
                source: module_id,
                target,
            });

            e.insert(target);
        }

        self.generator.imported_modules.get(&key).copied().unwrap()
    }
}
