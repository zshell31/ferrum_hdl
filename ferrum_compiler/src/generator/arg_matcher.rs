use rustc_middle::ty::{Const, ConstKind, GenericArg, GenericArgKind, Ty, TyKind};
use smallvec::SmallVec;

pub struct ArgMatcher<'tcx> {
    generic_args: SmallVec<[Option<GenericArg<'tcx>>; 8]>,
}

impl<'tcx> ArgMatcher<'tcx> {
    pub fn new() -> Self {
        let generic_args = SmallVec::with_capacity(8);

        Self { generic_args }
    }

    pub fn clear(&mut self) {
        self.generic_args.clear();
    }

    pub fn is_tys_match(&mut self, orig: &[Ty<'tcx>], with_param: &[Ty<'tcx>]) -> bool {
        if orig.len() != with_param.len() {
            return false;
        }

        orig.iter()
            .zip(with_param)
            .all(|(orig, with_param)| self.is_ty_match(*orig, *with_param))
    }

    pub fn is_args_match(
        &mut self,
        orig: &[GenericArg<'tcx>],
        with_param: &[GenericArg<'tcx>],
    ) -> bool {
        if orig.len() != with_param.len() {
            return false;
        }

        orig.iter()
            .zip(with_param)
            .all(|(orig, with_param)| self.is_arg_match(orig, with_param))
    }

    pub fn arg(&self, index: u32) -> Option<GenericArg<'tcx>> {
        let index = index as usize;
        self.generic_args.get(index).and_then(|arg| *arg)
    }

    fn is_arg_match(
        &mut self,
        orig: &GenericArg<'tcx>,
        with_param: &GenericArg<'tcx>,
    ) -> bool {
        let orig_kind = orig.unpack();
        let with_param_kind = with_param.unpack();

        match (orig_kind, with_param_kind) {
            (GenericArgKind::Type(orig_ty), GenericArgKind::Type(ty_with_param)) => {
                self.is_ty_match(orig_ty, ty_with_param)
            }
            (
                GenericArgKind::Const(orig_cons),
                GenericArgKind::Const(cons_with_param),
            ) => self.is_cons_match(orig_cons, cons_with_param),
            (GenericArgKind::Lifetime(_), GenericArgKind::Lifetime(_)) => {
                panic!("lifetimes are not supported");
            }
            _ => false,
        }
    }

    fn is_ty_match(&mut self, orig_ty: Ty<'tcx>, ty_with_param: Ty<'tcx>) -> bool {
        match ty_with_param.kind() {
            TyKind::Param(p) => {
                self.set_ty(p.index, orig_ty);
                true
            }
            _ => match (orig_ty.kind(), ty_with_param.kind()) {
                (TyKind::Adt(adt1, args1), TyKind::Adt(adt2, args2)) => {
                    adt1 == adt2 && self.is_args_match(args1, args2)
                }
                _ => {
                    println!(
                        "orig_ty: {:?}, ty_with_param: {:?}",
                        orig_ty, ty_with_param
                    );
                    false
                }
            },
        }
    }

    fn is_cons_match(
        &mut self,
        orig_cons: Const<'tcx>,
        cons_with_param: Const<'tcx>,
    ) -> bool {
        match cons_with_param.kind() {
            ConstKind::Param(p) => {
                self.set_cons(p.index, orig_cons);
                true
            }
            _ => orig_cons == cons_with_param,
        }
    }

    fn set_ty(&mut self, index: u32, ty: Ty<'tcx>) {
        self.reserve(index + 1);
        self.generic_args[index as usize] = Some(ty.into())
    }

    fn set_cons(&mut self, index: u32, cons: Const<'tcx>) {
        self.reserve(index + 1);
        self.generic_args[index as usize] = Some(cons.into());
    }

    fn reserve(&mut self, new_len: u32) {
        let new_len = new_len as usize;
        let l = self.generic_args.len();
        if l < new_len {
            self.generic_args.reserve(new_len);
            for _ in l .. new_len {
                self.generic_args.push(None);
            }
        }
    }
}
