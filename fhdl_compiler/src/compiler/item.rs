use std::{
    cell::{Ref, RefCell},
    fmt::{self, Debug},
    iter::{self, Peekable},
    mem,
    rc::Rc,
};

use either::Either;
use fhdl_data_structures::graph::{NodeId, Port};
use fhdl_netlist::{
    const_val::ConstVal,
    netlist::Module,
    node::{Merger, MergerArgs, Splitter, SplitterArgs},
    node_ty::NodeTy,
    symbol::Symbol,
};
use rustc_span::Span;
use rustc_target::abi::{FieldIdx, VariantIdx};
use smallvec::SmallVec;

use super::{
    item_ty::{ClosureTy, EnumTy, ItemTy, ItemTyKind},
    loop_gen::LoopGen,
    utils::{TreeIter, TreeNode},
    SymIdent,
};
use crate::error::{Error, SpanError, SpanErrorKind};

#[derive(Clone)]
pub struct Group<'tcx>(Rc<RefCell<SmallVec<[Item<'tcx>; 1]>>>);

impl<'tcx> Debug for Group<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.items().iter()).finish()
    }
}

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

    pub fn try_new_opt(
        items: impl IntoIterator<Item = Result<Option<Item<'tcx>>, Error>>,
    ) -> Result<Option<Self>, Error> {
        match items
            .into_iter()
            .collect::<Result<Option<SmallVec<[_; 1]>>, _>>()
        {
            Ok(Some(v)) => Ok(Some(Self::new(v))),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.borrow().len()
    }

    fn nodes(&self) -> usize {
        self.0.borrow().iter().map(|item| item.nodes()).sum()
    }

    #[inline]
    pub fn by_idx(&self, idx: usize) -> Item<'tcx> {
        self.0.borrow()[idx].clone()
    }

    #[inline]
    unsafe fn by_idx_mut(&mut self, idx: usize) -> &mut Item<'tcx> {
        // Check if we can borrow it mutably
        let _ = self.0.borrow_mut();

        let v = unsafe { self.0.as_ptr().as_mut().unwrap() };
        &mut v[idx]
    }

    pub fn slice(&self, start: usize, len: usize) -> Self {
        let items = &self.0.borrow()[start .. start + len];
        let items = items.iter().cloned();
        Self::new(items)
    }

    #[inline]
    pub fn items(&self) -> Ref<'_, [Item<'tcx>]> {
        Ref::map(self.0.borrow(), |vec| vec.as_slice())
    }

    pub fn to_iter(&self) -> GroupIter<'tcx> {
        let len = self.len();
        GroupIter {
            group: self.clone(),
            idx: 0,
            len,
        }
    }

    fn to_iter_mut<'a>(&'a mut self) -> GroupIterMut<'a, 'tcx>
    where
        'tcx: 'a,
    {
        let len = self.len();
        GroupIterMut {
            group: self,
            idx: 0,
            len,
        }
    }

    fn deep_clone(&self) -> Self {
        Self::new(self.0.borrow().iter().map(|item| item.deep_clone()))
    }
}

pub struct GroupIter<'tcx> {
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

pub struct GroupIterMut<'a, 'tcx: 'a> {
    group: &'a mut Group<'tcx>,
    idx: usize,
    len: usize,
}

impl<'a, 'tcx: 'a> Iterator for GroupIterMut<'a, 'tcx> {
    type Item = &'a mut Item<'tcx>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < self.len {
            let res = unsafe {
                let item = self.group.by_idx_mut(self.idx);
                mem::transmute::<_, &'a mut Item<'tcx>>(item)
            };
            self.idx += 1;
            Some(res)
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub enum ItemKind<'tcx> {
    Port(Port),
    Group(Group<'tcx>),
    Const(ConstVal),
    Reg,
    Option(Option<Rc<Item<'tcx>>>),
    LoopGen(LoopGen<'tcx>),
}

impl<'tcx> Debug for ItemKind<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Port(port) => write!(f, "port {port}"),
            Self::Group(group) => Debug::fmt(group, f),
            Self::Const(val) => Debug::fmt(val, f),
            Self::Reg => f.write_str("reg"),
            Self::Option(opt) => Debug::fmt(opt, f),
            Self::LoopGen(loop_gen) => Debug::fmt(loop_gen, f),
        }
    }
}

impl<'tcx> ItemKind<'tcx> {
    fn deep_clone(&self) -> Self {
        match self {
            Self::Port(node_out_id) => Self::Port(*node_out_id),
            Self::Group(group) => Self::Group(group.deep_clone()),
            Self::Const(val) => Self::Const(*val),
            Self::Reg => Self::Reg,
            Self::Option(opt) => Self::Option(opt.clone()),
            Self::LoopGen(loop_gen) => Self::LoopGen(loop_gen.clone()),
        }
    }

    fn nodes(&self) -> usize {
        match self {
            Self::Port(_) => 1,
            Self::Group(group) => group.nodes(),
            Self::Const(_) | Self::Reg | Self::Option(_) | Self::LoopGen(_) => 0,
        }
    }

    fn loop_gen_opt(&self) -> Option<&LoopGen<'tcx>> {
        match self {
            Self::LoopGen(loop_gen) => Some(loop_gen),
            _ => None,
        }
    }

    fn opt_opt(&self) -> Option<Option<&Rc<Item<'tcx>>>> {
        match self {
            Self::Option(opt) => Some(opt.as_ref()),
            _ => None,
        }
    }

    fn const_opt(&self) -> Option<&ConstVal> {
        match self {
            Self::Const(cons) => Some(cons),
            _ => None,
        }
    }

    fn is_loop_gen(&self) -> bool {
        matches!(self, Self::LoopGen(_))
    }

    fn is_option(&self) -> bool {
        matches!(self, Self::Option(_))
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

impl<'tcx> From<ConstVal> for ItemKind<'tcx> {
    fn from(val: ConstVal) -> Self {
        Self::Const(val)
    }
}

impl<'tcx> From<LoopGen<'tcx>> for ItemKind<'tcx> {
    fn from(loop_gen: LoopGen<'tcx>) -> Self {
        Self::LoopGen(loop_gen)
    }
}

#[derive(Debug, Clone)]
pub struct Item<'tcx> {
    pub ty: ItemTy<'tcx>,
    pub kind: ItemKind<'tcx>,
    pub nodes: usize,
}

impl<'tcx> Item<'tcx> {
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

    pub fn width(&self) -> u128 {
        self.ty.width()
    }

    pub fn group(&self) -> &Group<'tcx> {
        match &self.kind {
            ItemKind::Group(group) => group,
            _ => panic!("expected group"),
        }
    }

    pub fn group_mut(&mut self) -> &mut Group<'tcx> {
        match &mut self.kind {
            ItemKind::Group(group) => group,
            _ => panic!("expected group"),
        }
    }

    pub fn by_idx(&self, idx: usize) -> Item<'tcx> {
        self.group().by_idx(idx)
    }

    /// Check that two clones of the same item are not modified simultaneously
    /// The following code is invalid:
    /// ```ignore
    /// let mut a = ...;
    /// let mut b = a.clone();
    ///
    /// let a_mut = a.by_idx_mut(0);
    /// let b_mut = b.by_idx_mut(1);
    /// ````
    pub unsafe fn by_idx_mut(&mut self, idx: usize) -> &mut Item<'tcx> {
        self.group_mut().by_idx_mut(idx)
    }

    pub fn by_field(&self, idx: FieldIdx) -> Item<'tcx> {
        self.group().by_idx(idx.as_usize())
    }

    /// Check that two clones of the same item are not modified simultaneously
    /// The following code is invalid:
    /// ```ignore
    /// let mut a = ...;
    /// let mut b = a.clone();
    ///
    /// let a_mut = a.by_field_mut(0);
    /// let b_mut = b.by_field_mut(1);
    /// ````
    pub unsafe fn by_field_mut(&mut self, idx: FieldIdx) -> &mut Item<'tcx> {
        self.group_mut().by_idx_mut(idx.as_usize())
    }

    pub fn loop_gen_opt(&self) -> Option<&LoopGen<'tcx>> {
        self.kind.loop_gen_opt()
    }

    pub fn opt_opt(&self) -> Option<Option<&Rc<Item<'tcx>>>> {
        self.kind.opt_opt()
    }

    pub fn const_opt(&self) -> Option<&ConstVal> {
        self.kind.const_opt()
    }

    pub fn is_loop_gen(&self) -> bool {
        self.kind.is_loop_gen()
    }

    pub fn is_option(&self) -> bool {
        self.kind.is_option()
    }

    pub fn deep_clone(&self) -> Self {
        Self::new(self.ty, self.kind.deep_clone())
    }

    pub fn ports(&self) -> TreeIter<ItemIter<'tcx>> {
        let nodes = self.nodes();
        TreeIter::new(self.clone(), nodes)
    }

    /// Check that two clones of the same item are not modified simultaneously
    /// The following code is invalid:
    /// ```ignore
    /// let mut a = ...;
    /// let mut b = a.clone();
    ///
    /// let a_mut = a.by_field_mut(0);
    /// for port in b.ports_mut() {
    ///     ...
    /// }
    /// ````
    pub unsafe fn ports_mut<'a>(&'a mut self) -> TreeIter<ItemIterMut<'a, 'tcx>>
    where
        'tcx: 'a,
    {
        let nodes = self.nodes();
        TreeIter::new(self, nodes)
    }

    #[inline]
    pub fn is_unsigned(&self) -> bool {
        self.ty.is_unsigned()
    }
}

pub enum ItemIter<'tcx> {
    Empty,
    Port(Option<Port>),
    Group(GroupIter<'tcx>),
}

impl<'tcx> Iterator for ItemIter<'tcx> {
    type Item = TreeNode<Port, Item<'tcx>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::Port(port) => port.take().map(TreeNode::Leaf),
            Self::Group(group) => group.next().map(TreeNode::Node),
        }
    }
}

impl<'tcx> IntoIterator for Item<'tcx> {
    type Item = TreeNode<Port, Item<'tcx>>;
    type IntoIter = ItemIter<'tcx>;

    fn into_iter(self) -> Self::IntoIter {
        match self.kind {
            ItemKind::Port(port) => ItemIter::Port(Some(port)),
            ItemKind::Group(group) => ItemIter::Group(group.to_iter()),
            ItemKind::Const(_)
            | ItemKind::Reg
            | ItemKind::Option(_)
            | ItemKind::LoopGen(_) => ItemIter::Empty,
        }
    }
}

pub enum ItemIterMut<'a, 'tcx: 'a> {
    Empty,
    Port(Option<&'a mut Port>),
    Group(GroupIterMut<'a, 'tcx>),
}

impl<'a, 'tcx: 'a> Iterator for ItemIterMut<'a, 'tcx> {
    type Item = TreeNode<&'a mut Port, &'a mut Item<'tcx>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::Port(port) => port.take().map(TreeNode::Leaf),
            Self::Group(group) => group.next().map(TreeNode::Node),
        }
    }
}

impl<'a, 'tcx: 'a> IntoIterator for &'a mut Item<'tcx> {
    type Item = TreeNode<&'a mut Port, &'a mut Item<'tcx>>;
    type IntoIter = ItemIterMut<'a, 'tcx>;

    fn into_iter(self) -> Self::IntoIter {
        match &mut self.kind {
            ItemKind::Port(port) => ItemIterMut::Port(Some(port)),
            ItemKind::Group(group) => ItemIterMut::Group(group.to_iter_mut()),
            ItemKind::Const(_)
            | ItemKind::Reg
            | ItemKind::Option(_)
            | ItemKind::LoopGen(_) => ItemIterMut::Empty,
        }
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

pub struct CombineOutputs<'m, O: Iterator<Item = Port>> {
    module: &'m mut Module,
    outputs: Peekable<O>,
}

impl<'m> CombineOutputs<'m, CombineOutputsIter<'m>> {
    pub fn from_node(module: &'m mut Module, node_id: NodeId) -> Self {
        let outputs = module.node_out_ports(node_id).peekable();
        Self { module, outputs }
    }
}

impl<'m, O: Iterator<Item = Port>> CombineOutputs<'m, O> {
    pub fn next_output<'tcx>(
        &mut self,
        item_ty: ItemTy<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        match item_ty.kind() {
            ItemTyKind::Node(node_ty) => {
                let output = self.outputs.next().unwrap();
                assert_eq!(node_ty.width(), self.module[output].width());
                Ok(Item::new(item_ty, ItemKind::Port(output)))
            }
            ItemTyKind::Array(ty) => Ok(Item::new(
                item_ty,
                ItemKind::Group(Group::try_new(
                    ty.tys().map(|item_ty| self.next_output(item_ty, span)),
                )?),
            )),
            ItemTyKind::Struct(ty) | ItemTyKind::Closure(ClosureTy { ty, .. }) => {
                Ok(Item::new(
                    item_ty,
                    ItemKind::Group(Group::try_new(
                        ty.tys().map(|item_ty| self.next_output(item_ty, span)),
                    )?),
                ))
            }
            ItemTyKind::Enum(_) => Ok(Item::new(
                item_ty,
                ItemKind::Port(self.outputs.next().unwrap()),
            )),
            ItemTyKind::Reg(_) | ItemTyKind::Option(_) | ItemTyKind::LoopGen => {
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
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

    fn combine_from_node(
        &mut self,
        node_id: NodeId,
        item_ty: ItemTy<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error>;

    #[allow(clippy::wrong_self_convention)]
    fn to_bitvec(&mut self, item: &Item<'tcx>, span: Span) -> Result<Item<'tcx>, Error>;

    #[allow(clippy::wrong_self_convention)]
    fn from_bitvec(
        &mut self,
        item: impl ExtractPort,
        item_ty: ItemTy<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error>;

    fn enum_variant_from_bitvec(
        &mut self,
        scrutinee: Port,
        enum_ty: EnumTy<'tcx>,
        variant_idx: VariantIdx,
        span: Span,
    ) -> Result<Item<'tcx>, Error>;

    fn enum_variant_to_bitvec(
        &mut self,
        data_part: Option<Item<'tcx>>,
        enum_ty: ItemTy<'tcx>,
        variant_idx: VariantIdx,
        span: Span,
    ) -> Result<Item<'tcx>, Error>;

    fn get_discr(&mut self, discr: &Item<'tcx>, span: Span) -> Result<Item<'tcx>, Error>;

    fn mk_item_from_ty(
        &mut self,
        ty: ItemTy<'tcx>,
        mk_node: &impl Fn(NodeTy, &mut Module) -> Option<Port>,
        span: Span,
    ) -> Result<Option<Item<'tcx>>, Error>;

    fn mk_zero_sized_val(
        &mut self,
        ty: ItemTy<'tcx>,
        span: Span,
    ) -> Result<Option<Item<'tcx>>, Error>;

    fn to_const_val(&self, item: &Item<'tcx>) -> Option<u128>;
}

impl<'tcx> ModuleExt<'tcx> for Module {
    fn assign_names_to_item(&mut self, ident: &str, item: &Item, force: bool) {
        match &item.kind {
            ItemKind::Port(port) => {
                let port = *port;
                let sym = Some(Symbol::intern(ident));

                if self[port].sym.is_none() || (force && !self.is_mod_input(port)) {
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
            ItemKind::Const(_)
            | ItemKind::Reg
            | ItemKind::Option(_)
            | ItemKind::LoopGen(_) => {}
        }
    }

    fn combine_from_node(
        &mut self,
        node_id: NodeId,
        item_ty: ItemTy<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let mut outputs = CombineOutputs::from_node(self, node_id);
        let res = outputs.next_output(item_ty, span)?;
        assert!(!outputs.has_outputs());
        Ok(res)
    }

    #[allow(clippy::wrong_self_convention)]
    fn to_bitvec(&mut self, item: &Item<'tcx>, span: Span) -> Result<Item<'tcx>, Error> {
        match &item.kind {
            ItemKind::Port(_) => Ok(item.clone()),
            ItemKind::Group(group) => {
                if group.len() == 1 {
                    let item = group.by_idx(0);
                    self.to_bitvec(&item, span)
                } else {
                    let inputs = group
                        .items()
                        .iter()
                        .map(|item| self.to_bitvec(item, span).map(|item| item.port()))
                        .collect::<Result<SmallVec<[_; 1]>, _>>()?;

                    Ok(Item::new(
                        item.ty,
                        self.add_and_get_port::<_, Merger>(MergerArgs {
                            inputs,
                            rev: false,
                            sym: None,
                        }),
                    ))
                }
            }
            ItemKind::Const(val) => {
                let node_ty = item
                    .ty
                    .node_ty_opt()
                    .ok_or_else(|| SpanError::new(SpanErrorKind::NotSynthExpr, span))?;

                let item = Item::new(item.ty, self.const_val(node_ty, val.val()));
                self.to_bitvec(&item, span)
            }
            ItemKind::Reg | ItemKind::Option(_) | ItemKind::LoopGen(_) => {
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
            }
        }
    }

    #[allow(clippy::wrong_self_convention)]
    fn from_bitvec(
        &mut self,
        item: impl ExtractPort,
        item_ty: ItemTy<'tcx>,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let port = item.port();

        let node_width = self[port].ty.width();
        let item_width = item_ty.width();
        assert_eq!(node_width, item_width);

        let sym = self[port].sym;

        match item_ty.kind() {
            ItemTyKind::Node(_) | ItemTyKind::Enum(_) => {
                Ok(Item::new(item_ty, ItemKind::Port(port)))
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

                Ok(Item::new(
                    item_ty,
                    ItemKind::Group(Group::try_new(
                        outputs.map(|output| self.from_bitvec(output, ty.ty(), span)),
                    )?),
                ))
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

                Ok(Item::new(
                    item_ty,
                    ItemKind::Group(Group::try_new(
                        outputs.map(|(output, ty)| self.from_bitvec(output, ty, span)),
                    )?),
                ))
            }
            ItemTyKind::Reg(_) | ItemTyKind::Option(_) | ItemTyKind::LoopGen => {
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
            }
        }
    }

    fn enum_variant_from_bitvec(
        &mut self,
        scrutinee: Port,
        enum_ty: EnumTy<'tcx>,
        variant_idx: VariantIdx,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
        let variant = enum_ty.by_variant_idx(variant_idx);

        let splitter = SplitterArgs {
            input: scrutinee,
            outputs: iter::once((variant.ty.to_bitvec(), SymIdent::EnumPart.into())),
            start: Some(enum_ty.data_width()),
            rev: true,
        };

        let data_part = self.add_and_get_port::<_, Splitter>(splitter);

        self.from_bitvec(data_part, *variant.ty, span)
    }

    fn enum_variant_to_bitvec(
        &mut self,
        data_part: Option<Item<'tcx>>,
        enum_ty: ItemTy<'tcx>,
        variant_idx: VariantIdx,
        span: Span,
    ) -> Result<Item<'tcx>, Error> {
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
                Some(data_part) => self.to_bitvec(&data_part, span)?.port(),
                None => self.const_zero(NodeTy::BitVec(data_width)),
            };

            Either::Right([discr, data_part].into_iter())
        };

        let merger = MergerArgs {
            inputs,
            rev: false,
            sym: None,
        };

        Ok(Item::new(
            enum_ty,
            self.add_and_get_port::<_, Merger>(merger),
        ))
    }

    fn get_discr(&mut self, discr: &Item<'tcx>, span: Span) -> Result<Item<'tcx>, Error> {
        match &discr.kind {
            ItemKind::Port(_) if discr.ty.is_enum_ty() => {
                let enum_ty = discr.ty.enum_ty();
                let discr_ty = enum_ty.discr_ty();
                let discr = self.to_bitvec(discr, span)?;

                Ok(Item::new(
                    discr_ty,
                    self.add_and_get_port::<_, Splitter>(SplitterArgs {
                        input: discr.port(),
                        outputs: iter::once((discr_ty.node_ty(), SymIdent::Discr.into())),
                        start: None,
                        rev: true,
                    }),
                ))
            }
            ItemKind::Port(_)
            | ItemKind::Group(_)
            | ItemKind::Const(_)
            | ItemKind::Reg
            | ItemKind::Option(_) => Ok(discr.clone()),
            ItemKind::LoopGen(_) => {
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
            }
        }
    }

    fn mk_item_from_ty(
        &mut self,
        item_ty: ItemTy<'tcx>,
        mk_node: &impl Fn(NodeTy, &mut Module) -> Option<Port>,
        span: Span,
    ) -> Result<Option<Item<'tcx>>, Error> {
        match &item_ty.kind() {
            ItemTyKind::Node(node_ty) => Ok(mk_node(*node_ty, self)
                .map(|node_out_id| Item::new(item_ty, ItemKind::Port(node_out_id)))),
            ItemTyKind::Array(array_ty) => {
                match Group::try_new_opt(
                    array_ty
                        .tys()
                        .map(|ty| self.mk_item_from_ty(ty, mk_node, span)),
                ) {
                    Ok(Some(group)) => {
                        Ok(Some(Item::new(item_ty, ItemKind::Group(group))))
                    }
                    Ok(None) => Ok(None),
                    Err(e) => Err(e),
                }
            }
            ItemTyKind::Struct(struct_ty)
            | ItemTyKind::Closure(ClosureTy { ty: struct_ty, .. }) => {
                match Group::try_new_opt(
                    struct_ty
                        .tys()
                        .map(|ty| self.mk_item_from_ty(ty, mk_node, span)),
                ) {
                    Ok(Some(group)) => {
                        Ok(Some(Item::new(item_ty, ItemKind::Group(group))))
                    }
                    Ok(None) => Ok(None),
                    Err(e) => Err(e),
                }
            }
            ItemTyKind::Enum(_) => {
                let node_ty = item_ty.to_bitvec();
                Ok(mk_node(node_ty, self)
                    .map(|node_out_id| Item::new(item_ty, ItemKind::Port(node_out_id))))
            }
            ItemTyKind::Reg(_) => Ok(Some(Item::new(item_ty, ItemKind::Reg))),
            ItemTyKind::Option(_) | ItemTyKind::LoopGen => {
                Err(SpanError::new(SpanErrorKind::NotSynthExpr, span).into())
            }
        }
    }

    fn mk_zero_sized_val(
        &mut self,
        ty: ItemTy<'tcx>,
        span: Span,
    ) -> Result<Option<Item<'tcx>>, Error> {
        self.mk_item_from_ty(
            ty,
            &|node_ty, module| {
                if node_ty.is_zero_sized() {
                    Some(module.const_zero(node_ty))
                } else {
                    None
                }
            },
            span,
        )
    }

    fn to_const_val(&self, item: &Item<'tcx>) -> Option<u128> {
        let mut acc = ConstVal::default();
        for port in item.ports() {
            let val = self.to_const(port);
            let val = val?;
            acc.shift(val);
        }

        Some(acc.val())
    }
}

#[cfg(test)]
mod tests {
    use fhdl_data_structures::index::IndexType;

    use super::*;
    use crate::compiler::{
        domain::DomainId,
        item_ty::{RegTy, WithTypeInfo},
    };

    #[test]
    fn item_node_iter() {
        let node_id = NodeId::new(0);

        assert_eq!(
            Item::new(
                ItemTy::new(&WithTypeInfo::new(ItemTyKind::Node(NodeTy::Unsigned(8)))),
                ItemKind::Port(Port::new(node_id, 0))
            )
            .ports()
            .collect::<Vec<_>>(),
            &[Port::new(node_id, 0)]
        );
    }

    #[test]
    fn item_group_iter() {
        let node_id = NodeId::new(0);
        let ty = WithTypeInfo::new(ItemTyKind::Node(NodeTy::Unsigned(8)));
        let ty = ItemTy::new(&ty);

        assert_eq!(
            Item::new(
                ty,
                ItemKind::Group(Group::new([
                    Item::new(ty, ItemKind::Port(Port::new(node_id, 3))),
                    Item::new(
                        ty,
                        ItemKind::Group(Group::new([
                            Item::new(ty, ItemKind::Port(Port::new(node_id, 1))),
                            Item::new(
                                ty,
                                ItemKind::Group(Group::new([
                                    Item::new(ty, ItemKind::Port(Port::new(node_id, 2))),
                                    Item::new(ty, ItemKind::Port(Port::new(node_id, 0)))
                                ]))
                            )
                        ]))
                    )
                ]))
            )
            .ports()
            .collect::<Vec<_>>(),
            &[
                Port::new(node_id, 3),
                Port::new(node_id, 1),
                Port::new(node_id, 2),
                Port::new(node_id, 0)
            ]
        );
    }

    #[test]
    fn item_empty_iter() {
        let node_ty = WithTypeInfo::new(ItemTyKind::Node(NodeTy::Unsigned(8)));
        let node_ty = ItemTy::new(&node_ty);

        let ty = WithTypeInfo::new(ItemTyKind::Reg(RegTy {
            dom_id: DomainId::empty(),
            ty: node_ty,
        }));
        let ty = ItemTy::new(&ty);

        assert_eq!(Item::new(ty, ItemKind::Reg).ports().collect::<Vec<_>>(), &[
        ]);
    }

    #[test]
    fn item_empty_group_iter() {
        assert_eq!(
            Item::new(
                ItemTy::new(&WithTypeInfo::new(ItemTyKind::Node(NodeTy::Unsigned(8)))),
                ItemKind::Group(Group::new([]))
            )
            .ports()
            .collect::<Vec<_>>(),
            &[]
        );
    }
}
