use std::{
    cell::{Ref, RefCell, RefMut},
    iter::{self, Peekable},
    rc::Rc,
};

use either::Either;
use fhdl_netlist::{
    const_val::ConstVal,
    netlist::{IterMut, Module, NodeId, Port},
    node::{Merger, MergerArgs, Splitter, SplitterArgs},
    node_ty::NodeTy,
    symbol::Symbol,
};
use rustc_target::abi::{FieldIdx, VariantIdx};
use smallvec::SmallVec;

use super::{
    item_ty::{ClosureTy, EnumTy, ItemTy, ItemTyKind},
    SymIdent,
};
use crate::error::Error;

#[derive(Debug, Clone)]
pub struct Group<'tcx>(Rc<RefCell<SmallVec<[Item<'tcx>; 1]>>>);

impl<'tcx> Group<'tcx> {
    pub fn new(items: impl IntoIterator<Item = Item<'tcx>>) -> Self {
        Self(Rc::new(RefCell::new(items.into_iter().collect())))
    }

    pub fn new_opt(items: impl IntoIterator<Item = Option<Item<'tcx>>>) -> Option<Self> {
        let v = items.into_iter().collect::<Option<SmallVec<[_; 1]>>>()?;

        Some(Self::new(v))
    }

    pub fn try_new(
        items: impl IntoIterator<Item = Result<Item<'tcx>, Error>>,
    ) -> Result<Self, Error> {
        let v = items.into_iter().collect::<Result<SmallVec<[_; 1]>, _>>()?;

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

    fn next_back(&mut self) -> Option<Item<'tcx>> {
        if self.idx < self.len {
            let rev_idx = self.len - self.idx - 1;
            let item = unsafe { self.group.0.borrow().get_unchecked(rev_idx).clone() };
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
    Port(Port),
    Group(Group<'tcx>),
}

impl<'tcx> ItemKind<'tcx> {
    fn deep_clone(&self) -> Self {
        match self {
            Self::Port(node_out_id) => Self::Port(*node_out_id),
            Self::Group(group) => Self::Group(group.deep_clone()),
        }
    }

    fn nodes(&self) -> usize {
        match self {
            Self::Port(_) => 1,
            Self::Group(group) => group.nodes(),
        }
    }
}

impl<'tcx> From<Port> for ItemKind<'tcx> {
    fn from(port: Port) -> Self {
        Self::Port(port)
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
    Port(Option<Port>),
    Stack(Vec<GroupIter<'tcx>>),
}

fn next<'tcx>(
    stack: &mut Vec<GroupIter<'tcx>>,
    next: impl Fn(&mut GroupIter<'tcx>) -> Option<Item<'tcx>>,
) -> Option<Port> {
    while let Some(group) = stack.last_mut() {
        if let Some(item) = next(group) {
            match item.kind {
                ItemKind::Port(port) => {
                    return Some(port);
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

impl<'tcx> Iterator for ItemIter<'tcx> {
    type Item = Port;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Port(port) => port.take(),
            Self::Stack(stack) => next(stack, GroupIter::next),
        }
    }
}

impl<'tcx> DoubleEndedIterator for ItemIter<'tcx> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self {
            Self::Port(port) => port.take(),
            Self::Stack(stack) => next(stack, GroupIter::next_back),
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

    pub fn port(&self) -> Port {
        match &self.kind {
            ItemKind::Port(port) => *port,
            _ => panic!("expected port"),
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
            ItemKind::Port(node_out_id) => ItemIter::Port(Some(*node_out_id)),
            ItemKind::Group(group) => ItemIter::Stack(vec![group.to_iter()]),
        }
    }

    pub fn traverse(&mut self, mut f: impl FnMut(&mut Port)) -> &mut Self {
        match &mut self.kind {
            ItemKind::Port(node_out_id) => f(node_out_id),
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
                            ItemKind::Port(node_out_id) => f(node_out_id),
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

pub trait ExtractPort {
    fn port(&self) -> Port;
}

impl ExtractPort for Port {
    fn port(&self) -> Port {
        *self
    }
}

impl<'tcx> ExtractPort for Item<'tcx> {
    fn port(&self) -> Port {
        Item::port(self)
    }
}

impl<'tcx, 'a> ExtractPort for &'a Item<'tcx> {
    fn port(&self) -> Port {
        Item::port(self)
    }
}

pub type CombineOutputsIter<'m> = impl Iterator<Item = Port> + 'm;

pub struct CombineOutputs<'m> {
    module: &'m Module,
    outputs: Peekable<CombineOutputsIter<'m>>,
}

impl<'m> CombineOutputs<'m> {
    pub fn new(module: &'m Module, node_id: NodeId) -> Self {
        Self {
            module,
            outputs: module.node_out_ports(node_id).peekable(),
        }
    }

    pub fn next_output<'tcx>(&mut self, item_ty: ItemTy<'tcx>) -> Item<'tcx> {
        match item_ty.kind() {
            ItemTyKind::Node(node_ty) => {
                let output = self.outputs.next().unwrap();
                assert_eq!(node_ty.width(), self.module[output].width());
                Item::new(item_ty, ItemKind::Port(output))
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
                Item::new(item_ty, ItemKind::Port(self.outputs.next().unwrap()))
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

pub trait ModuleExt<'tcx> {
    fn assign_names_to_item(&mut self, ident: &str, item: &Item, force: bool);

    fn combine_outputs(&self, node_id: NodeId, item_ty: ItemTy<'tcx>) -> Item<'tcx>;

    #[allow(clippy::wrong_self_convention)]
    fn to_bitvec(&mut self, item: &Item<'tcx>) -> Item<'tcx>;

    #[allow(clippy::wrong_self_convention)]
    fn from_bitvec(
        &mut self,
        item: impl ExtractPort,
        item_ty: ItemTy<'tcx>,
    ) -> Item<'tcx>;

    fn enum_variant_from_bitvec(
        &mut self,
        scrutinee: Port,
        enum_ty: EnumTy<'tcx>,
        variant_idx: VariantIdx,
    ) -> Item<'tcx>;

    fn enum_variant_to_bitvec(
        &mut self,
        data_part: Option<Item<'tcx>>,
        enum_ty: ItemTy<'tcx>,
        variant_idx: VariantIdx,
    ) -> Item<'tcx>;

    fn mk_item_from_ty(
        &mut self,
        ty: ItemTy<'tcx>,
        mk_node: &impl Fn(NodeTy, &mut Module) -> Option<Port>,
    ) -> Option<Item<'tcx>>;

    fn mk_zero_sized_val(&mut self, ty: ItemTy<'tcx>) -> Option<Item<'tcx>>;

    fn to_const_val(&self, item: &Item<'tcx>) -> Option<u128>;
}

impl<'tcx> ModuleExt<'tcx> for Module {
    fn assign_names_to_item(&mut self, ident: &str, item: &Item, force: bool) {
        match &item.kind {
            ItemKind::Port(port) => {
                let port = *port;
                let sym = Some(Symbol::intern(ident));

                if self[port].sym.is_none() || force {
                    self[port].sym = sym;
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

    fn combine_outputs(&self, node_id: NodeId, item_ty: ItemTy<'tcx>) -> Item<'tcx> {
        let mut outputs = CombineOutputs::new(self, node_id);
        let res = outputs.next_output(item_ty);
        assert!(!outputs.has_outputs());
        res
    }

    #[allow(clippy::wrong_self_convention)]
    fn to_bitvec(&mut self, item: &Item<'tcx>) -> Item<'tcx> {
        match &item.kind {
            ItemKind::Port(_) => item.clone(),
            ItemKind::Group(group) => {
                if group.len() == 1 {
                    let item = group.by_idx(0);
                    self.to_bitvec(&item)
                } else {
                    let width = item.width();

                    let items = group.items();
                    let merger = MergerArgs {
                        width,
                        inputs: IterMut::new(items.iter(), |module, item| {
                            module.to_bitvec(item).port()
                        }),
                        rev: false,
                        sym: None,
                    };

                    Item::new(item.ty, self.add_and_get_port::<_, Merger>(merger))
                }
            }
        }
    }

    #[allow(clippy::wrong_self_convention)]
    fn from_bitvec(
        &mut self,
        item: impl ExtractPort,
        item_ty: ItemTy<'tcx>,
    ) -> Item<'tcx> {
        let port = item.port();

        let node_width = self[port].ty.width();
        let item_width = item_ty.width();
        assert_eq!(node_width, item_width);

        let sym = self[port].sym;

        match item_ty.kind() {
            ItemTyKind::Node(_) | ItemTyKind::Enum(_) => {
                Item::new(item_ty, ItemKind::Port(port))
            }
            ItemTyKind::Array(ty) => {
                let outputs = if ty.count() == 1 {
                    Either::Left(iter::once(port))
                } else {
                    let splitter = SplitterArgs {
                        input: port,
                        outputs: ty.tys().enumerate().map(|(idx, _)| {
                            let ident =
                                sym.map(|sym| Symbol::intern(tuple_field_name(sym, idx)));

                            (ty.ty().to_bitvec(), ident)
                        }),
                        start: None,
                        rev: true,
                    };
                    let splitter = self.add::<_, Splitter>(splitter);

                    Either::Right(self.node_out_ports(splitter))
                };

                Item::new(
                    item_ty,
                    ItemKind::Group(Group::new(
                        outputs.map(|output| self.from_bitvec(output, ty.ty())),
                    )),
                )
            }
            ItemTyKind::Struct(ty) | ItemTyKind::Closure(ClosureTy { ty, .. }) => {
                let outputs = if ty.len() == 1 {
                    Either::Left(iter::once(port).zip(ty.tys()))
                } else {
                    let splitter = SplitterArgs {
                        input: port,
                        outputs: ty.named_tys().map(|ty| {
                            let ident = sym.map(|sym| {
                                Symbol::intern(struct_field_name(sym, ty.name))
                            });
                            (ty.to_bitvec(), ident)
                        }),
                        start: None,
                        rev: true,
                    };
                    let splitter = self.add::<_, Splitter>(splitter);

                    Either::Right(self.node_out_ports(splitter).zip(ty.tys()))
                };

                Item::new(
                    item_ty,
                    ItemKind::Group(Group::new(
                        outputs.map(|(output, ty)| self.from_bitvec(output, ty)),
                    )),
                )
            }
        }
    }

    fn enum_variant_from_bitvec(
        &mut self,
        scrutinee: Port,
        enum_ty: EnumTy<'tcx>,
        variant_idx: VariantIdx,
    ) -> Item<'tcx> {
        let variant = enum_ty.by_variant_idx(variant_idx);

        let splitter = SplitterArgs {
            input: scrutinee,
            outputs: iter::once((variant.ty.to_bitvec(), SymIdent::EnumPart.into())),
            start: Some(enum_ty.data_width()),
            rev: true,
        };

        let data_part = self.add_and_get_port::<_, Splitter>(splitter);

        self.from_bitvec(data_part, *variant.ty)
    }

    fn enum_variant_to_bitvec(
        &mut self,
        data_part: Option<Item<'tcx>>,
        enum_ty: ItemTy<'tcx>,
        variant_idx: VariantIdx,
    ) -> Item<'tcx> {
        let (discr, data_width) = {
            let enum_ty = enum_ty.enum_ty();

            let variant = enum_ty.by_variant_idx(variant_idx);
            let discr = variant.discr;
            let discr_ty = enum_ty.discr_ty().node_ty();

            (self.const_val(discr_ty, discr), enum_ty.data_width())
        };

        let inputs = if data_width == 0 {
            Either::Left(iter::once(discr))
        } else {
            let data_part = match data_part {
                Some(data_part) => self.to_bitvec(&data_part).port(),
                None => self.const_zero(NodeTy::BitVec(data_width)),
            };

            Either::Right([discr, data_part].into_iter())
        };

        let merger = MergerArgs {
            width: enum_ty.width(),
            inputs,
            rev: false,
            sym: None,
        };

        Item::new(enum_ty, self.add_and_get_port::<_, Merger>(merger))
    }

    fn mk_item_from_ty(
        &mut self,
        ty: ItemTy<'tcx>,
        mk_node: &impl Fn(NodeTy, &mut Module) -> Option<Port>,
    ) -> Option<Item<'tcx>> {
        Some(match &ty.kind() {
            ItemTyKind::Node(node_ty) => {
                let node_out_id = mk_node(*node_ty, self)?;
                Item::new(ty, ItemKind::Port(node_out_id))
            }
            ItemTyKind::Array(array_ty) => Item::new(
                ty,
                ItemKind::Group(Group::new_opt(
                    array_ty.tys().map(|ty| self.mk_item_from_ty(ty, mk_node)),
                )?),
            ),
            ItemTyKind::Struct(struct_ty)
            | ItemTyKind::Closure(ClosureTy { ty: struct_ty, .. }) => Item::new(
                ty,
                ItemKind::Group(Group::new_opt(
                    struct_ty.tys().map(|ty| self.mk_item_from_ty(ty, mk_node)),
                )?),
            ),
            ItemTyKind::Enum(_) => {
                let node_ty = ty.to_bitvec();
                let node_out_id = mk_node(node_ty, self)?;
                Item::new(ty, ItemKind::Port(node_out_id))
            }
        })
    }

    fn mk_zero_sized_val(&mut self, ty: ItemTy<'tcx>) -> Option<Item<'tcx>> {
        self.mk_item_from_ty(ty, &|node_ty, module| {
            if node_ty.is_zero_sized() {
                Some(module.const_zero(node_ty))
            } else {
                None
            }
        })
    }

    fn to_const_val(&self, item: &Item<'tcx>) -> Option<u128> {
        let mut acc = ConstVal::default();
        for port in item.iter() {
            let val = self.to_const(port);
            let val = val?;
            acc.shift(val);
        }

        Some(acc.val())
    }
}

#[cfg(test)]
mod tests {
    use fhdl_netlist::netlist::IndexType;

    use super::*;
    use crate::compiler::item_ty::WithTypeInfo;

    #[test]
    fn item_node_iter() {
        let old_node_id = NodeId::new(0);
        let new_node_id = NodeId::new(1);

        assert_eq!(
            Item::new(
                ItemTy::new(&WithTypeInfo::new(ItemTyKind::Node(NodeTy::Unsigned(8)))),
                ItemKind::Port(Port::new(old_node_id, 0))
            )
            .traverse(|port| {
                *port = port.with(new_node_id);
            })
            .iter()
            .collect::<Vec<_>>(),
            &[Port::new(new_node_id, 0)]
        );
    }

    #[test]
    fn item_group_iter() {
        let old_node_id = NodeId::new(0);
        let new_node_id = NodeId::new(1);
        let ty = WithTypeInfo::new(ItemTyKind::Node(NodeTy::Unsigned(8)));
        let ty = ItemTy::new(&ty);

        assert_eq!(
            Item::new(
                ty,
                ItemKind::Group(Group::new([
                    Item::new(ty, ItemKind::Port(Port::new(old_node_id, 3))),
                    Item::new(
                        ty,
                        ItemKind::Group(Group::new([
                            Item::new(ty, ItemKind::Port(Port::new(old_node_id, 1))),
                            Item::new(
                                ty,
                                ItemKind::Group(Group::new([
                                    Item::new(
                                        ty,
                                        ItemKind::Port(Port::new(old_node_id, 2))
                                    ),
                                    Item::new(
                                        ty,
                                        ItemKind::Port(Port::new(old_node_id, 0))
                                    )
                                ]))
                            )
                        ]))
                    )
                ]))
            )
            .traverse(|port| {
                *port = port.with(new_node_id);
            })
            .iter()
            .collect::<Vec<_>>(),
            &[
                Port::new(new_node_id, 3),
                Port::new(new_node_id, 1),
                Port::new(new_node_id, 2),
                Port::new(new_node_id, 0)
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
            .traverse(|port| {
                *port = port.with(NodeId::new(1));
            })
            .iter()
            .collect::<Vec<_>>(),
            &[]
        );
    }
}
