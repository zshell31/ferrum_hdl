use fhdl_netlist::node_ty::NodeTy;
use rustc_abi::Size;
use rustc_const_eval::interpret::{alloc_range, Scalar};
use rustc_middle::{
    mir::{ConstValue, UnevaluatedConst},
    ty::{Const, ParamEnv, ScalarInt, Ty},
};
use rustc_span::Span;
use tracing::{debug, error};

use super::{item::Item, Compiler, Context};
use crate::{
    compiler::{item::Group, item_ty::ItemTyKind},
    error::{Error, SpanError, SpanErrorKind},
};

pub fn const_val_to_u128(val: ConstValue) -> Option<u128> {
    let scalar = val.try_to_scalar_int()?;

    scalar_int_to_u128(scalar)
}

pub fn scalar_to_u128(scalar: Scalar) -> Option<u128> {
    match scalar {
        Scalar::Int(scalar) => scalar_int_to_u128(scalar),
        _ => None,
    }
}

pub fn scalar_int_to_u128(scalar: ScalarInt) -> Option<u128> {
    scalar
        .try_to_u128()
        .ok()
        .or_else(|| scalar.try_to_u64().ok().map(|n| n as u128))
        .or_else(|| scalar.try_to_u32().ok().map(|n| n as u128))
        .or_else(|| scalar.try_to_u16().ok().map(|n| n as u128))
        .or_else(|| scalar.try_to_u8().ok().map(|n| n as u128))
}

impl<'tcx> Compiler<'tcx> {
    pub fn resolve_unevaluated(
        &mut self,
        unevaluated: UnevaluatedConst<'tcx>,
        ty: Ty<'tcx>,
        ctx: &Context<'tcx>,
        span: Span,
    ) -> Option<Item<'tcx>> {
        debug!("resolve_unevaluated: unevaluated = {unevaluated:?} ty = {ty:?}");
        let ty = self
            .resolve_ty(ty, ctx.generic_args, span)
            .map_err(|e| error!("resolve_unevaluated: {e}"))
            .ok()?;

        match ty.kind() {
            ItemTyKind::Node(node_ty @ NodeTy::Unsigned(n)) if *n <= 128 => {
                let val = self.const_eval_resolve(unevaluated)?;
                let val = const_val_to_u128(val)?;

                Some(Item::new(
                    ty,
                    self.netlist.const_val(ctx.module_id, *node_ty, val),
                ))
            }
            ItemTyKind::Array(array_ty) => {
                if let ItemTyKind::Node(node_ty @ NodeTy::Unsigned(8)) =
                    array_ty.ty().kind()
                {
                    let value = self.const_eval_resolve(unevaluated)?;
                    if let ConstValue::Indirect {
                        alloc_id,
                        mut offset,
                    } = value
                    {
                        let alloc =
                            self.tcx.global_alloc(alloc_id).unwrap_memory().inner();

                        let count = array_ty.count() as usize;
                        if alloc.len() == count {
                            let item_ty = array_ty.ty();

                            let byte_size = Size::from_bytes(1);
                            return Some(Item::new(
                                ty,
                                Group::new_opt((0 .. count).map(|_| {
                                    let val = alloc
                                        .read_scalar(
                                            &self.tcx,
                                            alloc_range(offset, byte_size),
                                            false,
                                        )
                                        .ok();
                                    debug!("resolve_unevaluated: read val = {val:?}");

                                    let val = val?.to_u8().ok()? as u128;
                                    offset += byte_size;

                                    Some(Item::new(
                                        item_ty,
                                        self.netlist.const_val(
                                            ctx.module_id,
                                            *node_ty,
                                            val,
                                        ),
                                    ))
                                }))?,
                            ));
                        }
                    }
                }

                None
            }
            _ => None,
        }
    }

    pub fn const_eval_resolve(
        &self,
        unevaluated: UnevaluatedConst<'tcx>,
    ) -> Option<ConstValue<'tcx>> {
        let param_env = ParamEnv::reveal_all();
        let value = self.tcx.const_eval_resolve(param_env, unevaluated, None);
        value.ok()
    }

    pub fn eval_const(&self, const_: Const<'tcx>, span: Span) -> Result<u128, Error> {
        const_
            .try_eval_scalar_int(self.tcx, ParamEnv::reveal_all())
            .and_then(scalar_int_to_u128)
            .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthGenParam, span).into())
    }

    pub fn mk_const(
        &mut self,
        ty: Ty<'tcx>,
        value: u128,
        ctx: &Context<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let ty = self.resolve_ty(ty, ctx.generic_args, span)?;

        Ok(Item::new(
            ty,
            self.netlist.const_val(ctx.module_id, ty.to_bitvec(), value),
        ))
    }
}
