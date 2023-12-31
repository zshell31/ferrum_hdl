use std::iter;

use either::Either;
use fhdl_blackbox::BlackboxKind;
use fhdl_netlist::{
    bvm::BitVecMask,
    group::ItemId,
    net_list::ModuleId,
    sig_ty::{SignalTy, SignalTyKind},
    symbol::Symbol,
};
use rustc_ast::ast::LitKind;
use rustc_hir::{
    def::{DefKind, Res},
    DotDotPos, Expr, ExprKind, Pat, PatKind, Path, QPath, Ty as HirTy,
    TyKind as HirTyKind,
};
use rustc_hir_analysis::{astconv::AstConv, collect::ItemCtxt};
use rustc_middle::ty::VariantDef;
use rustc_span::source_map::Spanned;

use super::Generator;
use crate::{
    blackbox::bit::BitVal,
    error::{Error, SpanError, SpanErrorKind},
    eval_context::EvalContext,
    utils,
};

impl<'tcx> Generator<'tcx> {
    pub fn assign_names_to_item(&mut self, ident: &str, item_id: ItemId) {
        match item_id {
            ItemId::Node(node_out_id) => {
                let sym = Some(Symbol::new(ident));

                self.netlist[node_out_id].sym = sym;
            }
            ItemId::Group(group) => match group.sig_ty.kind {
                SignalTyKind::Enum(_)
                | SignalTyKind::Array(_)
                | SignalTyKind::Node(_) => {
                    if group.item_ids().len() == 1 {
                        self.assign_names_to_item(ident, group.item_ids()[0]);
                    } else {
                        for (idx, item_id) in group.item_ids().iter().enumerate() {
                            let ident = format!("{}${}", ident, idx);
                            self.assign_names_to_item(&ident, *item_id);
                        }
                    }
                }
                SignalTyKind::Struct(ty) => {
                    if group.item_ids().len() == 1 {
                        self.assign_names_to_item(ident, group.item_ids()[0]);
                    } else {
                        ty.tys().iter().zip(group.item_ids()).for_each(
                            |(ty, item_id)| {
                                let ident = format!("{}${}", ident, ty.name);
                                self.assign_names_to_item(&ident, *item_id);
                            },
                        );
                    }
                }
            },
        }
    }

    pub fn pattern_match(
        &mut self,
        pat: &Pat<'tcx>,
        item_id: ItemId,
        module_id: ModuleId,
    ) -> Result<(), Error> {
        match pat.kind {
            PatKind::Binding(..) => {
                let ident = utils::pat_ident(pat)?;
                self.assign_names_to_item(ident.as_str(), item_id);

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
                let _ = self.item_ty(item_id).opt_array_ty().ok_or_else(|| {
                    SpanError::new(SpanErrorKind::ExpectedArray, pat.span)
                })?;
                let items = item_id.group().item_ids();

                for (ind, pat) in before.iter().enumerate() {
                    self.pattern_match(pat, items[ind], module_id)?;
                }

                if wild.is_some() {
                    let before_count = before.len();
                    let after_count = after.len();
                    assert!(before_count + after_count <= items.len());

                    let offset = items.len() - after_count;
                    for (ind, pat) in after.iter().enumerate() {
                        self.pattern_match(pat, items[offset + ind], module_id)?;
                    }
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

    pub fn pattern_to_bitvec(
        &mut self,
        pat: &Pat<'tcx>,
        sig_ty: SignalTy,
        ctx: &EvalContext<'tcx>,
    ) -> Result<BitVecMask, Error> {
        let mut bvm = BitVecMask::default();
        self.pattern_to_bitvec_(pat, sig_ty, ctx, &mut bvm)?;

        Ok(bvm)
    }

    fn pattern_to_bitvec_(
        &mut self,
        pat: &Pat<'tcx>,
        sig_ty: SignalTy,
        ctx: &EvalContext<'tcx>,
        bvm: &mut BitVecMask,
    ) -> Result<(), Error> {
        let is_enum_ty = sig_ty.is_enum_ty();
        let (sig_ty, offset) = match sig_ty.opt_enum_ty() {
            Some(enum_ty) => match pat.kind {
                PatKind::Wild => {
                    let width = enum_ty.width().value();
                    bvm.shiftl(width);
                    bvm.set_mask(width);
                    return Ok(());
                }
                _ => {
                    let (_, variant_idx) = self.pattern_to_variant_def(pat, ctx)?;
                    let width = enum_ty.discr_width();
                    bvm.shiftl(width);
                    bvm.set_val(enum_ty.discr_val(variant_idx), width);

                    let sig_ty = enum_ty.variant(variant_idx).inner;
                    let offset = enum_ty.data_width().value() - sig_ty.width().value();

                    (sig_ty, Some(offset))
                }
            },
            None => (sig_ty, None),
        };

        let res = match pat.kind {
            PatKind::Binding(..) | PatKind::Wild => {
                let width = sig_ty.width();
                bvm.shiftl(width.value());
                bvm.set_mask(width.value());

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
                bvm.shiftl(width.value());
                bvm.set_val(*lit, width.value());

                Ok(())
            }
            PatKind::Path(QPath::Resolved(..)) if is_enum_ty => Ok(()),
            PatKind::Path(QPath::Resolved(
                None,
                Path {
                    res: Res::Def(DefKind::Const, def_id),
                    ..
                },
            )) => {
                if self.is_local_def_id(*def_id) {
                    if let Some(const_val) =
                        self.eval_const_val(*def_id, ctx, Some(pat.span))
                    {
                        let width = sig_ty.width().value();
                        bvm.shiftl(width);
                        bvm.set_val(const_val, width);
                        return Ok(());
                    }

                    Err(SpanError::new(SpanErrorKind::InvalidPattern, pat.span).into())
                } else {
                    let blackbox = self.find_blackbox(*def_id, pat.span)?;
                    let value = match blackbox.kind {
                        BlackboxKind::BitL => BitVal(false).bit_value(),
                        BlackboxKind::BitH => BitVal(true).bit_value(),
                        _ => {
                            return Err(SpanError::new(
                                SpanErrorKind::InvalidPattern,
                                pat.span,
                            )
                            .into());
                        }
                    };

                    let width = sig_ty.width().value();
                    bvm.shiftl(width);
                    bvm.set_val(value, width);

                    Ok(())
                }
            }
            PatKind::Slice(before, wild, after) => {
                let array_ty = sig_ty.opt_array_ty().ok_or_else(|| {
                    SpanError::new(SpanErrorKind::ExpectedArray, pat.span)
                })?;
                let item_ty = array_ty.item_ty();
                let width = item_ty.width().value();

                let count = array_ty.count() as usize;

                for pat in before {
                    self.pattern_to_bitvec_(pat, item_ty, ctx, bvm)?;
                }

                if wild.is_some() {
                    let before_count = before.len();
                    let after_count = after.len();
                    assert!(before_count + after_count <= count);

                    let rest = count - (before_count + after_count);
                    for _ in 0 .. rest {
                        bvm.shiftl(width);
                        bvm.set_mask(width);
                    }

                    for pat in after {
                        self.pattern_to_bitvec_(pat, item_ty, ctx, bvm)?;
                    }
                }

                Ok(())
            }
            PatKind::Struct(_, fields, _) => {
                let struct_ty = sig_ty.struct_ty();

                for sig_ty in struct_ty.tys() {
                    if let Some(field) =
                        fields.iter().find(|field| sig_ty.is(field.ident.as_str()))
                    {
                        self.pattern_to_bitvec_(field.pat, sig_ty.inner, ctx, bvm)?;
                    } else {
                        let width = sig_ty.inner.width().value();
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
                        Some(pat) => self.pattern_to_bitvec_(pat, sig_ty, ctx, bvm)?,
                        None => {
                            let width = sig_ty.width().value();
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
        ctx: &EvalContext<'tcx>,
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
                let ty = self.type_of(*alias_to, ctx);
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
