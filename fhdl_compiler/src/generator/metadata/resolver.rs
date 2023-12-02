use fhdl_netlist::{
    net_list::{Idx, ModuleId, ParamId, TyId},
    node::{Node, NodeKind},
    resolver::{Resolve, Resolver},
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
    ctx: &'g EvalContext<'tcx>,
    span: Span,
}

impl<'g, 'tcx> MetadataResolver<'g, 'tcx> {
    pub fn new(
        generator: &'g mut Generator<'tcx>,
        metadata: &'g Metadata<'tcx>,
        fn_did: DefId,
        ctx: &'g EvalContext<'tcx>,
        span: Span,
    ) -> Self {
        Self {
            generator,
            metadata,
            fn_did,
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

    pub fn import_module_from_metadata(
        &mut self,
        source_id: ModuleId,
    ) -> Result<ModuleId, Error> {
        let netlist = self.metadata.netlist();
        let source = &netlist[source_id];
        let target_id = self.generator.netlist.add_module(source.name, false);
        self.generator.netlist[target_id].is_inlined = source.is_inlined;

        self.generator.imported_modules.insert(
            ModuleKey {
                krate: self.fn_did.krate,
                module_id: source_id,
            },
            target_id,
        );

        let offset = netlist.last_idx(source_id);
        self.generator.netlist.shift_last_idx(target_id, offset);

        let mut cursor = netlist.mod_cursor(source_id);
        while let Some(node_id) = netlist.next(&mut cursor) {
            let kind = netlist[node_id].resolve_kind(self)?;

            let kind = match kind {
                NodeKind::TemplateNode(node) => {
                    let id = node.temp_node_id();
                    let gen_node = self.metadata.template_node(id).ok_or_else(|| {
                        SpanError::new(SpanErrorKind::MissingTemplateNode(id), self.span)
                    })?;
                    let gen_node = gen_node.resolve(self)?;

                    gen_node.eval(target_id, self.generator)
                }
                _ => kind,
            };

            let node_id = node_id.with_module_id(target_id);
            let node = Node::new(node_id, kind);

            self.generator.netlist.add_node(target_id, node);
        }

        for node_out_id in netlist.mod_outputs(source_id) {
            let new_node_out_id = node_out_id.with_module_id(target_id);
            self.generator.netlist.add_output(new_node_out_id);
        }

        Ok(target_id)
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

    fn resolve_module_id(
        &mut self,
        module_id: ModuleId,
    ) -> Result<ModuleId, Self::Error> {
        let krate = self.fn_did.krate;
        let key = ModuleKey { krate, module_id };

        match self.generator.imported_modules.get(&key) {
            Some(target) => Ok(*target),
            None => self.import_module_from_metadata(module_id),
        }
    }
}
