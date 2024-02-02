use std::{
    iter::{self, Peekable},
    rc::Rc,
};

use either::Either;
use fhdl_netlist::{
    net_list::{ModuleId, NodeId, NodeOutId},
    node::{Merger, Splitter},
    node_ty::NodeTy,
    symbol::Symbol,
};
use rustc_target::abi::{FieldIdx, VariantIdx};

use super::{
    item_ty::{EnumTy, ItemTy, ItemTyKind},
    Compiler,
};
use crate::error::Error;

#[derive(Debug, Clone)]
pub struct Group<'tcx>(Rc<Vec<Item<'tcx>>>);

#[derive(Clone)]
struct GroupIter<'tcx> {
    group: Group<'tcx>,
    idx: usize,
    len: usize,
}

impl<'tcx> Iterator for GroupIter<'tcx> {
    type Item = Item<'tcx>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < self.len {
            let res = self.group.by_idx(self.idx).clone();
            self.idx += 1;
            Some(res)
        } else {
            None
        }
    }
}

impl<'tcx> Group<'tcx> {
    pub fn new(items: impl IntoIterator<Item = Item<'tcx>>) -> Self {
        Self(Rc::new(items.into_iter().collect()))
    }

    pub fn try_new(
        items: impl IntoIterator<Item = Result<Item<'tcx>, Error>>,
    ) -> Result<Self, Error> {
        let v = items.into_iter().collect::<Result<Vec<_>, _>>()?;

        Ok(Self::new(v))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn by_idx(&self, idx: usize) -> &Item<'tcx> {
        &self.0[idx]
    }

    #[inline]
    pub fn by_field(&self, idx: FieldIdx) -> &Item<'tcx> {
        self.by_idx(idx.as_usize())
    }

    #[inline]
    pub fn items(&self) -> &[Item<'tcx>] {
        self.0.as_slice()
    }

    fn into_iter(self) -> GroupIter<'tcx> {
        let len = self.len();
        GroupIter {
            group: self,
            idx: 0,
            len,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind<'tcx> {
    Node(NodeOutId),
    Module,
    Group(Group<'tcx>),
}

impl<'tcx> From<NodeOutId> for ItemKind<'tcx> {
    fn from(node_out_id: NodeOutId) -> Self {
        Self::Node(node_out_id)
    }
}

impl<'tcx> From<Group<'tcx>> for ItemKind<'tcx> {
    fn from(group: Group<'tcx>) -> Self {
        Self::Group(group)
    }
}

#[derive(Debug, Clone)]
pub struct Item<'tcx> {
    pub ty: ItemTy<'tcx>,
    pub kind: ItemKind<'tcx>,
}

impl<'tcx> Item<'tcx> {
    pub fn new(ty: ItemTy<'tcx>, kind: impl Into<ItemKind<'tcx>>) -> Self {
        Self {
            ty,
            kind: kind.into(),
        }
    }

    pub fn node_out_id(&self) -> NodeOutId {
        match &self.kind {
            ItemKind::Node(node_out_id) => *node_out_id,
            _ => panic!("expected node out id"),
        }
    }

    pub fn width(&self) -> u128 {
        self.ty.width()
    }

    pub fn group(&self) -> &Group<'tcx> {
        match &self.kind {
            ItemKind::Group(group) => group,
            _ => panic!("expected group"),
        }
    }

    pub fn by_idx(&self, idx: usize) -> &Item<'tcx> {
        self.group().by_idx(idx)
    }

    pub fn by_field(&self, idx: FieldIdx) -> &Item<'tcx> {
        self.group().by_field(idx)
    }

    pub fn iter(&self) -> impl Iterator<Item = NodeOutId> + 'tcx {
        enum StackEl<'tcx> {
            Item(Option<Item<'tcx>>),
            Iter(GroupIter<'tcx>),
        }

        impl<'tcx> StackEl<'tcx> {
            fn next(&mut self) -> Option<Item<'tcx>> {
                match self {
                    Self::Item(item) => item.take(),
                    Self::Iter(iter) => iter.next(),
                }
            }
        }

        impl<'tcx> From<Item<'tcx>> for StackEl<'tcx> {
            fn from(item: Item<'tcx>) -> Self {
                Self::Item(Some(item))
            }
        }

        impl<'tcx> From<Group<'tcx>> for StackEl<'tcx> {
            fn from(group: Group<'tcx>) -> Self {
                Self::Iter(group.into_iter())
            }
        }

        let mut stack: Vec<StackEl<'tcx>> = vec![self.clone().into()];

        iter::from_fn(move || loop {
            let item = stack.last_mut()?.next();

            if let Some(item) = item {
                let stack_el = match item.kind {
                    ItemKind::Node(node_out_id) => {
                        return Some(node_out_id);
                    }
                    ItemKind::Module => {
                        continue;
                    }
                    ItemKind::Group(group) => group.into(),
                };

                stack.push(stack_el);
            } else {
                stack.pop();
            }
        })
    }
}

pub trait ExtractNodeOutId {
    fn node_out_id(&self) -> NodeOutId;
}

impl ExtractNodeOutId for NodeOutId {
    fn node_out_id(&self) -> NodeOutId {
        *self
    }
}

impl<'tcx> ExtractNodeOutId for Item<'tcx> {
    fn node_out_id(&self) -> NodeOutId {
        Item::node_out_id(self)
    }
}

impl<'tcx, 'a> ExtractNodeOutId for &'a Item<'tcx> {
    fn node_out_id(&self) -> NodeOutId {
        Item::node_out_id(self)
    }
}

pub type CombineOutputsIter<'a> = impl Iterator<Item = NodeOutId> + 'a;

pub struct CombineOutputs<'a> {
    outputs: Peekable<CombineOutputsIter<'a>>,
}

impl<'a> CombineOutputs<'a> {
    pub fn new(compiler: &'a Compiler<'_>, node_id: NodeId) -> Self {
        Self {
            outputs: compiler.netlist[node_id].node_out_ids().peekable(),
        }
    }

    pub fn next_output<'tcx>(&mut self, item_ty: ItemTy<'tcx>) -> Item<'tcx> {
        match item_ty.kind() {
            ItemTyKind::Node(_) => {
                Item::new(item_ty, ItemKind::Node(self.outputs.next().unwrap()))
            }
            ItemTyKind::Module(_) => Item::new(item_ty, ItemKind::Module),
            ItemTyKind::Array(ty) => Item::new(
                item_ty,
                ItemKind::Group(Group::new(
                    ty.tys().map(|item_ty| self.next_output(item_ty)),
                )),
            ),
            ItemTyKind::Struct(ty) => Item::new(
                item_ty,
                ItemKind::Group(Group::new(
                    ty.tys().map(|item_ty| self.next_output(item_ty)),
                )),
            ),
            ItemTyKind::Enum(_) => {
                Item::new(item_ty, ItemKind::Node(self.outputs.next().unwrap()))
            }
        }
    }

    pub fn has_outputs(&mut self) -> bool {
        self.outputs.peek().is_some()
    }
}

impl<'tcx> Compiler<'tcx> {
    pub fn assign_names_to_item(&mut self, ident: &str, item: &Item, force: bool) {
        match &item.kind {
            ItemKind::Node(node_out_id) => {
                let node_out_id = *node_out_id;
                let sym = Some(Symbol::new(ident));

                if self.netlist[node_out_id].sym.is_none() || force {
                    self.netlist[node_out_id].sym = sym;
                }
            }
            ItemKind::Module => {}
            ItemKind::Group(group) => match item.ty.kind() {
                ItemTyKind::Node(_) | ItemTyKind::Array(_) | ItemTyKind::Enum(_) => {
                    if group.len() == 1 {
                        let item = group.by_idx(0);
                        self.assign_names_to_item(ident, item, force);
                    } else {
                        for (idx, item) in group.items().iter().enumerate() {
                            let ident = format!("{}${}", ident, idx);
                            self.assign_names_to_item(&ident, item, force);
                        }
                    }
                }
                ItemTyKind::Struct(ty) => {
                    if group.len() == 1 {
                        let item = group.by_idx(0);
                        self.assign_names_to_item(ident, item, force);
                    } else {
                        for (name, item) in ty.names().zip(group.items().iter()) {
                            let ident = format!("{}${}", ident, name);
                            self.assign_names_to_item(&ident, item, force);
                        }
                    }
                }
                _ => {}
            },
        }
    }

    pub fn combine_outputs(
        &mut self,
        node_id: NodeId,
        item_ty: ItemTy<'tcx>,
    ) -> Item<'tcx> {
        let mut outputs = CombineOutputs::new(self, node_id);
        let res = outputs.next_output(item_ty);
        assert!(!outputs.has_outputs());
        res
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_bitvec(&mut self, mod_id: ModuleId, item: &Item<'tcx>) -> Item<'tcx> {
        match &item.kind {
            ItemKind::Node(_) => item.clone(),
            ItemKind::Module => {
                Item::new(item.ty, self.netlist.const_zero(mod_id, NodeTy::BitVec(0)))
            }
            ItemKind::Group(group) => {
                if group.len() == 1 {
                    let item = group.by_idx(0);
                    self.to_bitvec(mod_id, item)
                } else {
                    let width = item.width();

                    let merger = Merger::new(
                        width,
                        group
                            .items()
                            .iter()
                            .map(|item_id| self.to_bitvec(mod_id, item_id).node_out_id()),
                        false,
                        None,
                    );

                    Item::new(item.ty, self.netlist.add_and_get_out(mod_id, merger))
                }
            }
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn from_bitvec(
        &mut self,
        mod_id: ModuleId,
        item: impl ExtractNodeOutId,
        item_ty: ItemTy<'tcx>,
    ) -> Item<'tcx> {
        let node_out_id = item.node_out_id();

        let node_width = self.netlist[node_out_id].ty.width();
        let item_width = item_ty.width();
        assert_eq!(node_width, item_width);

        match item_ty.kind() {
            ItemTyKind::Node(_) | ItemTyKind::Enum(_) => {
                Item::new(item_ty, ItemKind::Node(node_out_id))
            }
            ItemTyKind::Module(_) => Item::new(item_ty, ItemKind::Module),
            ItemTyKind::Array(ty) => {
                let outputs = if ty.count() == 1 {
                    Either::Left(iter::once(node_out_id))
                } else {
                    let splitter = self.netlist.add(
                        mod_id,
                        Splitter::new(
                            node_out_id,
                            ty.tys().map(|_| (ty.ty().to_bitvec(), None)),
                            None,
                            true,
                        ),
                    );

                    Either::Right(self.netlist[splitter].node_out_ids())
                };

                Item::new(
                    item_ty,
                    ItemKind::Group(Group::new(
                        outputs.map(|output| Item::new(ty.ty(), ItemKind::Node(output))),
                    )),
                )
            }
            ItemTyKind::Struct(ty) => {
                let outputs = if ty.len() == 1 {
                    Either::Left(iter::once(node_out_id).zip(ty.tys()))
                } else {
                    let splitter = self.netlist.add(
                        mod_id,
                        Splitter::new(
                            node_out_id,
                            ty.tys().map(|ty| (ty.to_bitvec(), None)),
                            None,
                            true,
                        ),
                    );

                    Either::Right(self.netlist[splitter].node_out_ids().zip(ty.tys()))
                };

                Item::new(
                    item_ty,
                    ItemKind::Group(Group::new(
                        outputs.map(|(output, ty)| Item::new(ty, ItemKind::Node(output))),
                    )),
                )
            }
        }
    }

    pub fn enum_variant_from_bitvec(
        &mut self,
        mod_id: ModuleId,
        scrutinee: NodeOutId,
        enum_ty: EnumTy<'tcx>,
        variant_idx: VariantIdx,
    ) -> Item<'tcx> {
        let variant_ty = enum_ty.by_variant_idx(variant_idx);

        let data_part = self.netlist.add_and_get_out(
            mod_id,
            Splitter::new(
                scrutinee,
                [(variant_ty.to_bitvec(), None)],
                Some(enum_ty.data_width()),
                true,
            ),
        );

        self.from_bitvec(mod_id, data_part, variant_ty)
    }

    pub fn enum_variant_to_bitvec(
        &mut self,
        mod_id: ModuleId,
        data_part: Option<Item<'tcx>>,
        enum_ty: ItemTy<'tcx>,
        variant_idx: VariantIdx,
    ) -> Item<'tcx> {
        let (discriminant, data_width) = {
            let enum_ty = enum_ty.enum_ty();

            let discriminant = enum_ty.discrimant(variant_idx);
            let discriminant_ty = NodeTy::BitVec(enum_ty.discr_width());

            (
                self.netlist
                    .const_val(mod_id, discriminant_ty, discriminant),
                enum_ty.data_width(),
            )
        };

        let inputs = if data_width == 0 {
            Either::Left(iter::once(discriminant))
        } else {
            let data_part = match data_part {
                Some(data_part) => self.to_bitvec(mod_id, &data_part).node_out_id(),
                None => self.netlist.const_zero(mod_id, NodeTy::BitVec(data_width)),
            };

            Either::Right([discriminant, data_part].into_iter())
        };

        Item::new(
            enum_ty,
            self.netlist.add_and_get_out(
                mod_id,
                Merger::new(enum_ty.width(), inputs, false, None),
            ),
        )
    }
}

#[cfg(test)]
mod tests {
    use fhdl_netlist::{
        net_list::{Idx, ModuleId, NodeId},
        node_ty::NodeTy,
    };

    use super::*;
    use crate::compiler::item_ty::ItemTyKind;

    #[test]
    fn item_node_iter() {
        let out_id = NodeOutId::new(NodeId::new(ModuleId::new(1), 1), 0);

        assert_eq!(
            Item::new(
                ItemTy::make(&ItemTyKind::Node(NodeTy::Unsigned(8))),
                ItemKind::Node(out_id)
            )
            .iter()
            .collect::<Vec<_>>(),
            &[out_id]
        );
    }

    #[test]
    fn item_module_iter() {
        let module_id = ModuleId::new(1);

        assert_eq!(
            Item::new(
                ItemTy::make(&ItemTyKind::Module(module_id)),
                ItemKind::Module
            )
            .iter()
            .collect::<Vec<_>>(),
            &[]
        );
    }

    #[test]
    fn item_group_iter() {
        let node_id = NodeId::new(ModuleId::new(1), 1);
        let out_id1 = NodeOutId::new(node_id, 0);
        let out_id2 = NodeOutId::new(node_id, 1);
        let out_id3 = NodeOutId::new(node_id, 2);
        let out_id4 = NodeOutId::new(node_id, 3);
        let ty = ItemTy::make(&ItemTyKind::Node(NodeTy::Unsigned(8)));

        assert_eq!(
            Item::new(
                ty,
                ItemKind::Group(Group::new([
                    Item::new(ty, ItemKind::Node(out_id4)),
                    Item::new(
                        ty,
                        ItemKind::Group(Group::new([
                            Item::new(ty, ItemKind::Node(out_id2)),
                            Item::new(
                                ty,
                                ItemKind::Group(Group::new([
                                    Item::new(ty, ItemKind::Node(out_id3)),
                                    Item::new(ty, ItemKind::Node(out_id1))
                                ]))
                            )
                        ]))
                    )
                ]))
            )
            .iter()
            .collect::<Vec<_>>(),
            &[out_id4, out_id2, out_id3, out_id1]
        );
    }

    #[test]
    fn item_empty_group_iter() {
        assert_eq!(
            Item::new(
                ItemTy::make(&ItemTyKind::Node(NodeTy::Unsigned(8))),
                ItemKind::Group(Group::new([]))
            )
            .iter()
            .collect::<Vec<_>>(),
            &[]
        );
    }
}
