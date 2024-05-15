use std::{cell::RefCell, iter, rc::Rc};

use derive_where::derive_where;
use fhdl_netlist::const_val::ConstVal;

use super::{
    item::{Item, ItemKind},
    item_ty::ItemTy,
    Compiler,
};
use crate::compiler::item::Group;

#[derive_where(Debug)]
#[derive(Clone)]
pub struct LoopGen<'tcx> {
    iter_item_ty: ItemTy<'tcx>,
    #[derive_where(skip)]
    iter: Rc<RefCell<dyn Iterator<Item = Item<'tcx>> + 'tcx>>,
    len: usize,
}

impl<'tcx> LoopGen<'tcx> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(
        compiler: &mut Compiler<'tcx>,
        iter_item_ty: ItemTy<'tcx>,
        iter: impl Iterator<Item = Item<'tcx>> + 'tcx,
        len: usize,
    ) -> Item<'tcx> {
        Item::new(compiler.loop_gen_ty(), Self {
            iter_item_ty,
            iter: Rc::new(RefCell::new(iter)),
            len,
        })
    }

    pub fn enumerate(&self, compiler: &mut Compiler<'tcx>) -> Item<'tcx> {
        let iter = self.iter.clone();

        let idx_ty = compiler.usize_ty();
        let iter_item_ty =
            compiler.alloc_tuple_ty([idx_ty, self.iter_item_ty].into_iter());

        let mut idx = 0;
        Self::new(
            compiler,
            iter_item_ty,
            iter::from_fn(move || {
                let res = iter.borrow_mut().next().map(|item| {
                    let idx = Item::new(idx_ty, ConstVal::new(idx, idx_ty.width()));
                    Item::new(iter_item_ty, Group::new([idx, item]))
                });
                idx += 1;

                res
            }),
            self.len,
        )
    }

    pub fn next(&self, compiler: &mut Compiler<'tcx>) -> Item<'tcx> {
        let item = self.iter.borrow_mut().next();
        Item::new(
            compiler.opt_ty(self.iter_item_ty),
            ItemKind::Option(item.map(Rc::new)),
        )
    }
}
