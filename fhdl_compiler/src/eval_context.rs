use fhdl_netlist::net_list::ModuleId;
use rustc_middle::ty::{EarlyBinder, GenericArgsRef, TyCtxt};
use rustc_type_ir::fold::TypeFoldable;

#[derive(Clone, Debug)]
pub struct EvalContext<'tcx> {
    pub is_primary: bool,
    pub generic_args: GenericArgsRef<'tcx>,
    pub module_id: ModuleId,
}

impl<'tcx> EvalContext<'tcx> {
    pub fn new(
        is_primary: bool,
        generic_args: GenericArgsRef<'tcx>,
        module_id: ModuleId,
    ) -> Self {
        Self {
            is_primary,
            generic_args,
            module_id,
        }
    }

    pub fn instantiate<T: TypeFoldable<TyCtxt<'tcx>>>(
        &self,
        tcx: TyCtxt<'tcx>,
        foldable: T,
    ) -> T {
        let binder = EarlyBinder::bind(foldable);
        self.instantiate_early_binder(tcx, binder)
    }

    pub fn instantiate_early_binder<T: TypeFoldable<TyCtxt<'tcx>>>(
        &self,
        tcx: TyCtxt<'tcx>,
        binder: EarlyBinder<T>,
    ) -> T {
        if self.is_primary {
            binder.instantiate(tcx, self.generic_args)
        } else {
            binder.instantiate_identity()
        }
    }

    pub fn with_generic_args(&self, generic_args: GenericArgsRef<'tcx>) -> Self {
        Self {
            is_primary: self.is_primary,
            generic_args,
            module_id: self.module_id,
        }
    }

    pub fn with_module_id(&self, module_id: ModuleId) -> Self {
        Self {
            is_primary: self.is_primary,
            generic_args: self.generic_args,
            module_id,
        }
    }
}
