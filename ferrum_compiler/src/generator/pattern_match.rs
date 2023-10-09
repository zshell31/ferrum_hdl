use std::iter;

use either::Either;
use ferrum_netlist::{
    group::ItemId,
    net_list::{ModuleId, NodeOutId},
    node::{BitVecMask, IsNode, Splitter},
    params::Outputs,
    sig_ty::{PrimTy, SignalTy},
};
use rustc_ast::ast::LitKind;
use rustc_hir::{
    def::{DefKind, Res},
    DotDotPos, Expr, ExprKind, Pat, PatKind, Path, QPath, Ty as HirTy,
    TyKind as HirTyKind,
};
use rustc_hir_analysis::{astconv::AstConv, collect::ItemCtxt};
use rustc_middle::ty::{GenericArgsRef, VariantDef};
use rustc_span::source_map::Spanned;

use super::Generator;
use crate::{
    error::{Error, SpanError, SpanErrorKind},
    utils,
};

impl<'tcx> Generator<'tcx> {
    pub fn pattern_match(
        &mut self,
        pat: &Pat<'tcx>,
        item_id: ItemId,
        module_id: ModuleId,
    ) -> Result<(), Error> {
        match pat.kind {
            PatKind::Binding(..) => {
                let ident = utils::pat_ident(pat)?;
                match item_id {
                    ItemId::Node(node_id) => {
                        let sym = self.idents.for_module(module_id).ident(ident.as_str());

                        self.net_list[node_id]
                            .kind
                            .outputs_mut()
                            .only_one_mut()
                            .out
                            .sym = sym;
                    }
                    ItemId::Group(group) => {
                        for item_id in group.item_ids() {
                            self.pattern_match(pat, *item_id, module_id)?;
                        }
                    }
                }

                self.idents
                    .for_module(module_id)
                    .add_local_ident(ident, item_id);
            }
            PatKind::Lit(Expr {
                kind:
                    ExprKind::Lit(Spanned {
                        node: LitKind::Int(_, _),
                        ..
                    }),
                ..
            }) => {}
            PatKind::Path(QPath::Resolved(_, _)) => {}
            PatKind::Slice(before, wild, after) => {
                let array_ty = self.item_ty(item_id).opt_array_ty().ok_or_else(|| {
                    SpanError::new(SpanErrorKind::ExpectedArray, pat.span)
                })?;
                let item_ty = array_ty.item_ty();
                let width = item_ty.width();

                let to = self.to_bitvec(module_id, item_id);

                self.slice_pattern_match(to, width, *item_ty, before, None)?;

                if wild.is_some() {
                    let start = (after.len() as u128) * width;
                    self.slice_pattern_match(to, width, *item_ty, after, Some(start))?;
                }
            }
            PatKind::Struct(_, fields, _) => {
                let group = item_id.group();
                assert!(fields.len() <= group.item_ids().len());

                for field in fields {
                    let field_item_id =
                        group.by_field(field.ident.as_str()).ok_or_else(|| {
                            SpanError::new(SpanErrorKind::InvalidPattern, field.span)
                        })?;

                    self.pattern_match(field.pat, field_item_id, module_id)?;
                }
            }
            PatKind::Tuple(pats, dot_dot_pos)
            | PatKind::TupleStruct(_, pats, dot_dot_pos) => {
                let group = item_id.group();

                for (pat, item_id) in
                    Self::zip_tuple_pats_with(pats, dot_dot_pos, group.item_ids())
                        .filter(|(pat, _)| pat.is_some())
                {
                    self.pattern_match(pat.unwrap(), *item_id, module_id)?;
                }
            }
            PatKind::Wild => {}
            _ => {
                println!("{:#?}", pat);
                return Err(
                    SpanError::new(SpanErrorKind::InvalidPattern, pat.span).into()
                );
            }
        }

        Ok(())
    }

    fn zip_tuple_pats_with<'a, T>(
        pats: &'a [Pat<'tcx>],
        dot_dot_pos: DotDotPos,
        items: &'a [T],
    ) -> impl Iterator<Item = (Option<&'a Pat<'tcx>>, &'a T)> + 'a {
        match dot_dot_pos.as_opt_usize() {
            Some(pos) => {
                assert!(pats.len() < items.len());

                let before_end = pos;
                let after_start = items.len() - (pats.len() - pos);
                let before = pats[0 .. pos].iter().map(Some).zip(&items[0 .. before_end]);
                let middle = iter::repeat(None).zip(&items[before_end .. after_start]);
                let after = pats[pos ..].iter().map(Some).zip(&items[after_start ..]);

                Either::Left(before.chain(middle).chain(after))
            }
            None => {
                assert_eq!(pats.len(), items.len());

                Either::Right(pats.iter().map(Some).zip(items))
            }
        }
    }

    fn slice_pattern_match(
        &mut self,
        node_out_id: NodeOutId,
        width: u128,
        sig_ty: SignalTy,
        pat: &[Pat<'tcx>],
        start: Option<u128>,
    ) -> Result<(), Error> {
        if pat.is_empty() {
            return Ok(());
        }

        let module_id = node_out_id.node_id().module_id();
        let splitter = Splitter::new(
            node_out_id,
            (0 .. pat.len()).map(|_| {
                (
                    PrimTy::BitVec(width),
                    self.idents.for_module(module_id).tmp(),
                )
            }),
            start,
            true,
        );
        let node_id = self.net_list.add_node(module_id, splitter);

        let outputs = self.net_list[node_id].kind.outputs();
        assert_eq!(outputs.len(), pat.len());

        let node_out_ids = outputs
            .items()
            .map(|out| out.node_out_id(node_id))
            .collect::<Vec<_>>();

        for (node_out_id, before_pat) in node_out_ids.into_iter().zip(pat) {
            let from = self.from_bitvec(module_id, node_out_id, sig_ty);
            self.pattern_match(before_pat, from, module_id)?;
        }

        Ok(())
    }

    pub fn pattern_to_bitvec(
        &mut self,
        pat: &Pat<'tcx>,
        sig_ty: SignalTy,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Result<BitVecMask, Error> {
        let mut bvm = BitVecMask::default();
        self.pattern_to_bitvec_(pat, sig_ty, generic_args, &mut bvm)?;

        Ok(bvm)
    }

    fn pattern_to_bitvec_(
        &self,
        pat: &Pat<'tcx>,
        sig_ty: SignalTy,
        generic_args: GenericArgsRef<'tcx>,
        bvm: &mut BitVecMask,
    ) -> Result<(), Error> {
        let is_enum_ty = sig_ty.is_enum_ty();
        let (sig_ty, offset) = match sig_ty.opt_enum_ty() {
            Some(enum_ty) => match pat.kind {
                PatKind::Wild => {
                    let width = enum_ty.width();
                    bvm.shiftl(width);
                    bvm.set_mask(width);
                    return Ok(());
                }
                _ => {
                    let (_, variant_idx) =
                        self.pattern_to_variant_def(pat, generic_args)?;
                    let width = enum_ty.discr_width();
                    bvm.shiftl(width);
                    bvm.set_val(enum_ty.discr_val(variant_idx), width);

                    let sig_ty = enum_ty.variant(variant_idx).inner;
                    let offset = enum_ty.data_width() - sig_ty.width();

                    (sig_ty, Some(offset))
                }
            },
            None => (sig_ty, None),
        };

        let res = match pat.kind {
            PatKind::Binding(..) | PatKind::Wild => {
                let width = sig_ty.width();
                bvm.shiftl(width);
                bvm.set_mask(width);

                Ok(())
            }
            PatKind::Lit(Expr {
                kind:
                    ExprKind::Lit(Spanned {
                        node: LitKind::Int(lit, _),
                        ..
                    }),
                ..
            }) => {
                let width = sig_ty.width();
                bvm.shiftl(width);
                bvm.set_val(*lit, width);

                Ok(())
            }
            PatKind::Path(QPath::Resolved(..)) if is_enum_ty => Ok(()),
            PatKind::Path(QPath::Resolved(
                None,
                Path {
                    res: Res::Def(DefKind::Const, def_id),
                    ..
                },
            )) if def_id.is_local() => {
                if let Some(const_val) =
                    self.eval_const_val(*def_id, generic_args, Some(pat.span))
                {
                    let width = sig_ty.width();
                    bvm.shiftl(width);
                    bvm.set_val(const_val, width);
                    return Ok(());
                }

                Err(SpanError::new(SpanErrorKind::InvalidPattern, pat.span).into())
            }
            PatKind::Struct(_, fields, _) => {
                let struct_ty = sig_ty.struct_ty();

                for sig_ty in struct_ty.tys() {
                    if let Some(field) =
                        fields.iter().find(|field| sig_ty.is(field.ident.as_str()))
                    {
                        self.pattern_to_bitvec_(
                            field.pat,
                            sig_ty.inner,
                            generic_args,
                            bvm,
                        )?;
                    } else {
                        let width = sig_ty.inner.width();
                        bvm.shiftl(width);
                        bvm.set_mask(width);
                    }
                }

                Ok(())
            }
            PatKind::TupleStruct(_, pats, dot_dot_pos) => {
                let struct_ty = sig_ty.struct_ty();
                for (pat, sig_ty) in
                    Self::zip_tuple_pats_with(pats, dot_dot_pos, struct_ty.tys())
                {
                    let sig_ty = sig_ty.inner;
                    match pat {
                        Some(pat) => {
                            self.pattern_to_bitvec_(pat, sig_ty, generic_args, bvm)?
                        }
                        None => {
                            let width = sig_ty.width();
                            bvm.shiftl(width);
                            bvm.set_mask(width);
                        }
                    }
                }
                Ok(())
            }
            _ => Err(SpanError::new(SpanErrorKind::InvalidPattern, pat.span).into()),
        };

        if res.is_ok() {
            if let Some(offset) = offset {
                bvm.shiftl(offset);
                bvm.set_mask(offset);
            }
        } else {
            println!("{:#?}", pat);
        }

        res
    }

    pub fn pattern_to_variant_def(
        &self,
        pat: &Pat<'tcx>,
        generic_args: GenericArgsRef<'tcx>,
    ) -> Result<(&'tcx VariantDef, usize), Error> {
        let variant_def = match pat.kind {
            PatKind::Path(QPath::Resolved(_, path))
            | PatKind::TupleStruct(QPath::Resolved(_, path), ..)
            | PatKind::Struct(QPath::Resolved(_, path), ..) => {
                Ok(self.tcx.expect_variant_res(path.res))
            }
            PatKind::TupleStruct(
                QPath::TypeRelative(
                    qself @ HirTy {
                        kind:
                            HirTyKind::Path(QPath::Resolved(
                                _,
                                Path {
                                    res: Res::SelfTyAlias { alias_to, .. },
                                    ..
                                },
                            )),
                        ..
                    },
                    assoc_segment,
                ),
                ..,
            ) => {
                let ty = self
                    .tcx
                    .type_of(alias_to)
                    .instantiate(self.tcx, generic_args);
                let item_ctx = &ItemCtxt::new(self.tcx, pat.hir_id.owner.def_id);
                let astconv = item_ctx.astconv();
                if let Ok((_, _, variant_did)) = astconv.associated_path_to_ty(
                    pat.hir_id,
                    pat.span,
                    ty,
                    qself,
                    assoc_segment,
                    true,
                ) {
                    Ok(self
                        .tcx
                        .adt_def(self.tcx.parent(variant_did))
                        .variant_with_id(variant_did))
                } else {
                    Err(SpanError::new(SpanErrorKind::ExpectedEnumVariant, pat.span)
                        .into())
                }
                // let adt_def = self.tcx.adt_def(alias_to);
                // if let TyKind::Adt(adt, _) = ty.kind() {
                //     println!("{:#?}", self.tcx.adt_def(adt.did()).variants());
                // }
            }
            _ => Err(SpanError::new(SpanErrorKind::ExpectedEnumVariant, pat.span).into()),
        };

        variant_def
            .map(|variant_def| {
                let adt = self.tcx.adt_def(self.tcx.parent(variant_def.def_id));
                let idx = adt.variant_index_with_id(variant_def.def_id);

                (variant_def, idx.as_usize())
            })
            .map_err(|e| {
                println!("{:#?}", pat);
                e
            })
    }
}
