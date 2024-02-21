use std::{
    cell::{Ref, RefCell, RefMut},
    iter::{self, Peekable},
    rc::Rc,
};

use either::Either;
use fhdl_netlist::{
    const_val::ConstVal,
    net_list::{ModuleId, NodeId, NodeOutId},
    node::{Merger, Splitter},
    node_ty::NodeTy,
    symbol::Symbol,
};
use rustc_target::abi::{FieldIdx, VariantIdx};

use super::{
    item_ty::{ClosureTy, EnumTy, ItemTy, ItemTyKind},
    Compiler, Context, SymIdent,
};
use crate::error::Error;

#[derive(Debug, Clone)]
pub struct Group<'tcx>(Rc<RefCell<Vec<Item<'tcx>>>>);

impl<'tcx> Group<'tcx> {
    pub fn new(items: impl IntoIterator<Item = Item<'tcx>>) -> Self {
        Self(Rc::new(RefCell::new(items.into_iter().collect())))
    }

    pub fn new_opt(items: impl IntoIterator<Item = Option<Item<'tcx>>>) -> Option<Self> {
        let v = items.into_iter().collect::<Option<Vec<_>>>()?;

        Some(Self::new(v))
    }

    pub fn try_new(
        items: impl IntoIterator<Item = Result<Item<'tcx>, Error>>,
    ) -> Result<Self, Error> {
        let v = items.into_iter().collect::<Result<Vec<_>, _>>()?;

        Ok(Self::new(v))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.borrow().len()
    }

    fn nodes(&self) -> usize {
        self.0.borrow().iter().map(|item| item.nodes()).sum()
    }

    #[inline]
    fn by_idx(&self, idx: usize) -> Item<'tcx> {
        self.0.borrow()[idx].clone()
    }

    #[inline]
    fn by_idx_mut(&mut self, idx: usize) -> &mut Item<'tcx> {
        // Check if we can borrow it mutably
        let _ = self.0.borrow_mut();

        let v = unsafe { self.0.as_ptr().as_mut().unwrap() };
        &mut v[idx]
    }

    #[inline]
    pub fn items(&self) -> Ref<'_, [Item<'tcx>]> {
        Ref::map(self.0.borrow(), |vec| vec.as_slice())
    }

    fn to_iter(&self) -> GroupIter<'tcx> {
        let len = self.len();
        GroupIter {
            group: self.clone(),
            idx: 0,
            len,
        }
    }

    fn deep_clone(&self) -> Self {
        Self::new(self.0.borrow().iter().map(|item| item.deep_clone()))
    }
}

#[derive(Clone)]
pub struct GroupIter<'tcx> {
    group: Group<'tcx>,
    idx: usize,
    len: usize,
}

impl<'tcx> GroupIter<'tcx> {
    fn next(&mut self) -> Option<Item<'tcx>> {
        if self.idx < self.len {
            let item = unsafe { self.group.0.borrow().get_unchecked(self.idx).clone() };
            self.idx += 1;
            Some(item)
        } else {
            None
        }
    }

    fn next_mut(&mut self) -> Option<RefMut<'_, Item<'tcx>>> {
        if self.idx < self.len {
            let item = RefMut::map(self.group.0.borrow_mut(), |vec| unsafe {
                vec.get_unchecked_mut(self.idx)
            });
            self.idx += 1;
            Some(item)
        } else {
            None
        }
    }
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

#[derive(Debug, Clone)]
pub enum ItemKind<'tcx> {
    Node(NodeOutId),
    Group(Group<'tcx>),
}

impl<'tcx> ItemKind<'tcx> {
    fn deep_clone(&self) -> Self {
        match self {
            Self::Node(node_out_id) => Self::Node(*node_out_id),
            Self::Group(group) => Self::Group(group.deep_clone()),
        }
    }

    fn nodes(&self) -> usize {
        match self {
            Self::Node(_) => 1,
            Self::Group(group) => group.nodes(),
        }
    }
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
    pub nodes: usize,
}

pub enum ItemIter<'tcx> {
    Node(Option<NodeOutId>),
    Stack(Vec<GroupIter<'tcx>>),
}

impl<'tcx> Iterator for ItemIter<'tcx> {
    type Item = NodeOutId;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Node(node_out_id) => node_out_id.take(),
            Self::Stack(stack) => {
                while let Some(group) = stack.last_mut() {
                    if let Some(item) = group.next() {
                        match item.kind {
                            ItemKind::Node(node_out_id) => {
                                return Some(node_out_id);
                            }
                            ItemKind::Group(group) => {
                                stack.push(group.to_iter());
                            }
                        }
                    } else {
                        stack.pop();
                    }
                }

                None
            }
        }
    }
}

impl<'tcx> Item<'tcx> {
    #[inline]
    pub fn new(ty: ItemTy<'tcx>, kind: impl Into<ItemKind<'tcx>>) -> Self {
        let kind = kind.into();
        let nodes = kind.nodes();

        Self { ty, kind, nodes }
    }

    pub fn nodes(&self) -> usize {
        self.nodes
    }

    pub fn node_out_id(&self) -> NodeOutId {
        match &self.kind {
            ItemKind::Node(node_out_id) => *node_out_id,
            _ => panic!("expected node out id"),
        }
    }

    #[inline]
    pub fn width(&self) -> u128 {
        self.ty.width()
    }

    #[inline]
    pub fn group(&self) -> &Group<'tcx> {
        match &self.kind {
            ItemKind::Group(group) => group,
            _ => panic!("expected group"),
        }
    }

    #[inline]
    pub fn group_mut(&mut self) -> &mut Group<'tcx> {
        match &mut self.kind {
            ItemKind::Group(group) => group,
            _ => panic!("expected group"),
        }
    }

    #[inline]
    pub fn by_idx(&self, idx: usize) -> Item<'tcx> {
        self.group().by_idx(idx)
    }

    #[inline]
    pub fn by_field(&self, idx: FieldIdx) -> Item<'tcx> {
        self.group().by_idx(idx.as_usize())
    }

    #[inline]
    pub fn by_field_mut(&mut self, idx: FieldIdx) -> &mut Item<'tcx> {
        self.group_mut().by_idx_mut(idx.as_usize())
    }

    #[inline]
    pub fn deep_clone(&self) -> Self {
        Self::new(self.ty, self.kind.deep_clone())
    }

    pub fn iter(&self) -> ItemIter<'tcx> {
        match &self.kind {
            ItemKind::Node(node_out_id) => ItemIter::Node(Some(*node_out_id)),
            ItemKind::Group(group) => ItemIter::Stack(vec![group.to_iter()]),
        }
    }

    pub fn traverse(&mut self, mut f: impl FnMut(&mut NodeOutId)) -> &mut Self {
        match &mut self.kind {
            ItemKind::Node(node_out_id) => f(node_out_id),
            ItemKind::Group(group) => {
                let mut stack: Vec<GroupIter<'tcx>> = vec![group.to_iter()];

                enum StackCmd<'tcx> {
                    Push(GroupIter<'tcx>),
                    Pop,
                }

                while let Some(group) = stack.last_mut() {
                    let mut cmd = None;

                    if let Some(mut item) = group.next_mut() {
                        match &mut item.kind {
                            ItemKind::Node(node_out_id) => f(node_out_id),
                            ItemKind::Group(group) => {
                                cmd = Some(StackCmd::Push(group.to_iter()));
                            }
                        }
                    } else {
                        cmd = Some(StackCmd::Pop);
                    }

                    if let Some(cmd) = cmd {
                        match cmd {
                            StackCmd::Push(group) => {
                                stack.push(group);
                            }
                            StackCmd::Pop => {
                                stack.pop();
                            }
                        }
                    }
                }
            }
        }

        self
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
            ItemTyKind::Array(ty) => Item::new(
                item_ty,
                ItemKind::Group(Group::new(
                    ty.tys().map(|item_ty| self.next_output(item_ty)),
                )),
            ),
            ItemTyKind::Struct(ty) | ItemTyKind::Closure(ClosureTy { ty, .. }) => {
                Item::new(
                    item_ty,
                    ItemKind::Group(Group::new(
                        ty.tys().map(|item_ty| self.next_output(item_ty)),
                    )),
                )
            }
            ItemTyKind::Enum(_) => {
                Item::new(item_ty, ItemKind::Node(self.outputs.next().unwrap()))
            }
        }
    }

    pub fn has_outputs(&mut self) -> bool {
        self.outputs.peek().is_some()
    }
}

fn tuple_field_name(ident: impl AsRef<str>, idx: usize) -> String {
    format!("{}${}", ident.as_ref(), idx)
}

fn struct_field_name(ident: impl AsRef<str>, name: impl AsRef<str>) -> String {
    format!("{}${}", ident.as_ref(), name.as_ref())
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
            ItemKind::Group(group) => match item.ty.kind() {
                ItemTyKind::Node(_) | ItemTyKind::Array(_) | ItemTyKind::Enum(_) => {
                    if group.len() == 1 {
                        let item = group.by_idx(0);
                        self.assign_names_to_item(ident, &item, force);
                    } else {
                        for (idx, item) in group.items().iter().enumerate() {
                            let ident = tuple_field_name(ident, idx);
                            self.assign_names_to_item(&ident, item, force);
                        }
                    }
                }
                ItemTyKind::Struct(ty) => {
                    if group.len() == 1 {
                        let item = group.by_idx(0);
                        self.assign_names_to_item(ident, &item, force);
                    } else {
                        for (name, item) in ty.names().zip(group.items().iter()) {
                            let ident = struct_field_name(ident, name);
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
            ItemKind::Group(group) => {
                if group.len() == 1 {
                    let item = group.by_idx(0);
                    self.to_bitvec(mod_id, &item)
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

        let sym = self.netlist[node_out_id].sym;

        match item_ty.kind() {
            ItemTyKind::Node(_) | ItemTyKind::Enum(_) => {
                Item::new(item_ty, ItemKind::Node(node_out_id))
            }
            ItemTyKind::Array(ty) => {
                let outputs = if ty.count() == 1 {
                    Either::Left(iter::once(node_out_id))
                } else {
                    let splitter = self.netlist.add(
                        mod_id,
                        Splitter::new(
                            node_out_id,
                            ty.tys().enumerate().map(|(idx, _)| {
                                let ident = sym
                                    .map(|sym| Symbol::new(tuple_field_name(sym, idx)));

                                (ty.ty().to_bitvec(), ident)
                            }),
                            None,
                            true,
                        ),
                    );

                    Either::Right(
                        self.netlist[splitter]
                            .node_out_ids()
                            .collect::<Vec<_>>()
                            .into_iter(),
                    )
                };

                Item::new(
                    item_ty,
                    ItemKind::Group(Group::new(
                        outputs.map(|output| self.from_bitvec(mod_id, output, ty.ty())),
                    )),
                )
            }
            ItemTyKind::Struct(ty) | ItemTyKind::Closure(ClosureTy { ty, .. }) => {
                let outputs = if ty.len() == 1 {
                    Either::Left(iter::once(node_out_id).zip(ty.tys()))
                } else {
                    let splitter = self.netlist.add(
                        mod_id,
                        Splitter::new(
                            node_out_id,
                            ty.named_tys().map(|ty| {
                                let ident = sym.map(|sym| {
                                    Symbol::new(struct_field_name(sym, ty.name))
                                });
                                (ty.to_bitvec(), ident)
                            }),
                            None,
                            true,
                        ),
                    );

                    Either::Right(
                        self.netlist[splitter]
                            .node_out_ids()
                            .zip(ty.tys())
                            .collect::<Vec<_>>()
                            .into_iter(),
                    )
                };

                Item::new(
                    item_ty,
                    ItemKind::Group(Group::new(
                        outputs.map(|(output, ty)| self.from_bitvec(mod_id, output, ty)),
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
        let variant = enum_ty.by_variant_idx(variant_idx);

        let data_part = self.netlist.add_and_get_out(
            mod_id,
            Splitter::new(
                scrutinee,
                [(variant.ty.to_bitvec(), SymIdent::EnumPart)],
                Some(enum_ty.data_width()),
                true,
            ),
        );

        self.from_bitvec(mod_id, data_part, *variant.ty)
    }

    pub fn enum_variant_to_bitvec(
        &mut self,
        mod_id: ModuleId,
        data_part: Option<Item<'tcx>>,
        enum_ty: ItemTy<'tcx>,
        variant_idx: VariantIdx,
    ) -> Item<'tcx> {
        let (discr, data_width) = {
            let enum_ty = enum_ty.enum_ty();

            let variant = enum_ty.by_variant_idx(variant_idx);
            let discr = variant.discr;
            let discr_ty = enum_ty.discr_ty().node_ty();

            (
                self.netlist.const_val(mod_id, discr_ty, discr),
                enum_ty.data_width(),
            )
        };

        let inputs = if data_width == 0 {
            Either::Left(iter::once(discr))
        } else {
            let data_part = match data_part {
                Some(data_part) => self.to_bitvec(mod_id, &data_part).node_out_id(),
                None => self.netlist.const_zero(mod_id, NodeTy::BitVec(data_width)),
            };

            Either::Right([discr, data_part].into_iter())
        };

        Item::new(
            enum_ty,
            self.netlist.add_and_get_out(
                mod_id,
                Merger::new(enum_ty.width(), inputs, false, None),
            ),
        )
    }

    pub fn mk_item_from_ty(
        &mut self,
        ty: ItemTy<'tcx>,
        ctx: &Context<'tcx>,
        mk_node: &impl Fn(&mut Compiler<'tcx>, NodeTy, &Context<'tcx>) -> Option<NodeOutId>,
    ) -> Option<Item<'tcx>> {
        Some(match &ty.kind() {
            ItemTyKind::Node(node_ty) => {
                let node_out_id = mk_node(self, *node_ty, ctx)?;
                Item::new(ty, ItemKind::Node(node_out_id))
            }
            ItemTyKind::Array(array_ty) => Item::new(
                ty,
                ItemKind::Group(Group::new_opt(
                    array_ty
                        .tys()
                        .map(|ty| self.mk_item_from_ty(ty, ctx, mk_node)),
                )?),
            ),
            ItemTyKind::Struct(struct_ty)
            | ItemTyKind::Closure(ClosureTy { ty: struct_ty, .. }) => Item::new(
                ty,
                ItemKind::Group(Group::new_opt(
                    struct_ty
                        .tys()
                        .map(|ty| self.mk_item_from_ty(ty, ctx, mk_node)),
                )?),
            ),
            ItemTyKind::Enum(_) => {
                let node_ty = ty.to_bitvec();
                let node_out_id = mk_node(self, node_ty, ctx)?;
                Item::new(ty, ItemKind::Node(node_out_id))
            }
        })
    }

    pub fn mk_zero_sized_val(
        &mut self,
        ty: ItemTy<'tcx>,
        ctx: &Context<'tcx>,
    ) -> Option<Item<'tcx>> {
        self.mk_item_from_ty(ty, ctx, &|compiler, node_ty, ctx| {
            if node_ty.is_zero_sized() {
                Some(compiler.netlist.const_zero(ctx.module_id, node_ty))
            } else {
                None
            }
        })
    }

    pub fn to_const(&self, item: &Item<'tcx>) -> Option<u128> {
        let mut acc = ConstVal::default();
        for node_out_id in item.iter() {
            let val = self.netlist.to_const(node_out_id);
            let val = val?;
            acc.shift(val);
        }

        Some(acc.val())
    }
}

#[cfg(test)]
mod tests {
    use fhdl_netlist::{
        net_list::{Idx, ModuleId, NodeId},
        node_ty::NodeTy,
    };

    use super::*;
    use crate::compiler::item_ty::{ItemTyKind, WithTypeInfo};

    #[test]
    fn item_node_iter() {
        let old_node_id = NodeId::new(ModuleId::new(1), 0);
        let new_node_id = NodeId::new(ModuleId::new(1), 1);

        assert_eq!(
            Item::new(
                ItemTy::new(&WithTypeInfo::new(ItemTyKind::Node(NodeTy::Unsigned(8)))),
                ItemKind::Node(NodeOutId::new(old_node_id, 0))
            )
            .traverse(|node_out_id| {
                *node_out_id = node_out_id.with_node_id(new_node_id);
            })
            .iter()
            .collect::<Vec<_>>(),
            &[NodeOutId::new(new_node_id, 0)]
        );
    }

    #[test]
    fn item_group_iter() {
        let old_node_id = NodeId::new(ModuleId::new(1), 0);
        let new_node_id = NodeId::new(ModuleId::new(1), 1);
        let ty = WithTypeInfo::new(ItemTyKind::Node(NodeTy::Unsigned(8)));
        let ty = ItemTy::new(&ty);

        assert_eq!(
            Item::new(
                ty,
                ItemKind::Group(Group::new([
                    Item::new(ty, ItemKind::Node(NodeOutId::new(old_node_id, 3))),
                    Item::new(
                        ty,
                        ItemKind::Group(Group::new([
                            Item::new(ty, ItemKind::Node(NodeOutId::new(old_node_id, 1))),
                            Item::new(
                                ty,
                                ItemKind::Group(Group::new([
                                    Item::new(
                                        ty,
                                        ItemKind::Node(NodeOutId::new(old_node_id, 2))
                                    ),
                                    Item::new(
                                        ty,
                                        ItemKind::Node(NodeOutId::new(old_node_id, 0))
                                    )
                                ]))
                            )
                        ]))
                    )
                ]))
            )
            .traverse(|node_out_id| {
                *node_out_id = node_out_id.with_node_id(new_node_id);
            })
            .iter()
            .collect::<Vec<_>>(),
            &[
                NodeOutId::new(new_node_id, 3),
                NodeOutId::new(new_node_id, 1),
                NodeOutId::new(new_node_id, 2),
                NodeOutId::new(new_node_id, 0)
            ]
        );
    }

    #[test]
    fn item_empty_group_iter() {
        assert_eq!(
            Item::new(
                ItemTy::new(&WithTypeInfo::new(ItemTyKind::Node(NodeTy::Unsigned(8)))),
                ItemKind::Group(Group::new([]))
            )
            .traverse(|node_out_id| {
                *node_out_id = node_out_id.with_node_id(NodeId::new(ModuleId::new(1), 1));
            })
            .iter()
            .collect::<Vec<_>>(),
            &[]
        );
    }
}
