mod bin_op;
mod bit_not;
mod case;
mod cons;
mod dff;
mod input;
mod merger;
mod mod_inst;
mod mux2;
mod not;
mod splitter;
mod temp_node;
mod zero_extend;

use std::mem;

use auto_enums::auto_enum;
use rustc_macros::{Decodable, Encodable};

pub(crate) use self::cons::MultiConst;
pub use self::{
    bin_op::{BinOp, BinOpNode},
    bit_not::BitNot,
    case::{Case, CaseInputs},
    cons::Const,
    dff::{DFFInputs, DFF},
    input::Input,
    merger::Merger,
    mod_inst::ModInst,
    mux2::{Mux2, Mux2Inputs},
    not::Not,
    splitter::{Indices, Splitter},
    temp_node::TemplateNode,
    zero_extend::ZeroExtend,
};
use crate::{
    const_val::ConstVal,
    net_list::{
        list::ListItem, Idx, InOut, ModuleId, NetList, NodeId, NodeIdx, NodeInId,
        NodeOutId, NodeOutIdx, WithId,
    },
    resolver::{Resolve, Resolver},
    sig_ty::{NodeTy, Width},
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, Encodable, Decodable)]
pub enum NetKind {
    Wire,
    Reg(usize),
}

#[derive(Debug, Clone, Copy, Encodable, Decodable)]
pub struct NodeOutput {
    pub ty: NodeTy,
    pub kind: NetKind,
    pub sym: Option<Symbol>,
    pub is_skip: bool,
    pub inject: bool,
}

impl NodeOutput {
    pub fn wire(ty: NodeTy, sym: Option<Symbol>) -> Self {
        Self::new(ty, NetKind::Wire, sym)
    }

    pub fn reg(ty: NodeTy, sym: Option<Symbol>, init: usize) -> Self {
        Self::new(ty, NetKind::Reg(init), sym)
    }

    fn new(ty: NodeTy, kind: NetKind, sym: Option<Symbol>) -> Self {
        Self {
            ty,
            kind,
            sym,
            is_skip: true,
            inject: false,
        }
    }
}

impl NodeOutput {
    #[inline]
    pub fn width(&self) -> Width {
        self.ty.width()
    }
}

impl<R: Resolver> Resolve<R> for NodeOutput {
    fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
        Ok(Self {
            ty: self.ty.resolve(resolver)?,
            ..*self
        })
    }
}

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct Node {
    pub is_skip: bool,
    pub inject: bool,
    kind: NodeKind,
    module_id: ModuleId,
    node_idx: NodeIdx,
    next: Option<NodeIdx>,
    prev: Option<NodeIdx>,
}

impl ListItem<NodeIdx> for Node {
    fn idx(&self) -> NodeIdx {
        self.node_idx
    }

    fn next(&self) -> Option<NodeIdx> {
        self.next
    }

    fn set_next(&mut self, next: Option<NodeIdx>) {
        self.next = next;
    }

    fn prev(&self) -> Option<NodeIdx> {
        self.prev
    }

    fn set_prev(&mut self, prev: Option<NodeIdx>) {
        self.prev = prev;
    }
}

impl Node {
    pub fn new(node_id: NodeId, kind: NodeKind) -> Self {
        let (module_id, node_idx) = node_id.split();

        Self {
            kind,
            is_skip: true,
            inject: false,
            module_id,
            node_idx,
            next: None,
            prev: None,
        }
    }

    pub fn resolve_kind<R: Resolver>(
        &self,
        resolver: &mut R,
    ) -> Result<NodeKind, R::Error> {
        self.kind.resolve(resolver)
    }

    pub fn kind(&self) -> NodeKindWithId<'_> {
        NodeKindWithId::new(self.module_id, &self.kind)
    }

    pub fn kind_mut(&mut self) -> NodeKindWithIdMut<'_> {
        NodeKindWithIdMut::new(self.module_id, &mut self.kind)
    }

    pub fn next(&self) -> Option<NodeId> {
        ListItem::next(self).map(|next| NodeId::combine(self.module_id, next))
    }

    pub fn set_next(&mut self, node_id: Option<NodeId>) {
        ListItem::set_next(
            self,
            node_id.map(|node_id| {
                let (module_id, node_idx) = node_id.split();
                assert_eq!(module_id, self.module_id);
                node_idx
            }),
        );
    }

    pub fn prev(&self) -> Option<NodeId> {
        ListItem::prev(self).map(|prev| NodeId::combine(self.module_id, prev))
    }

    pub fn set_prev(&mut self, node_id: Option<NodeId>) {
        ListItem::set_prev(
            self,
            node_id.map(|node_id| {
                let (module_id, node_idx) = node_id.split();
                assert_eq!(module_id, self.module_id);
                node_idx
            }),
        )
    }

    #[inline]
    pub fn node_id(&self) -> NodeId {
        NodeId::combine(self.module_id, self.node_idx)
    }

    pub fn inputs(&self) -> impl Iterator<Item = WithId<NodeInId, NodeOutId>> + '_ {
        let module_id = self.module_id;
        let node_id = self.node_id();
        InOut::<NodeOutIdx>::items(self.kind.inputs()).map(move |(ind, node_out_idx)| {
            WithId::new(
                NodeInId::new(node_id, ind),
                NodeOutId::make(module_id, *node_out_idx),
            )
        })
    }

    pub fn inputs_mut(
        &mut self,
    ) -> impl Iterator<Item = WithId<NodeInId, &mut NodeOutIdx>> + '_ {
        let node_id = self.node_id();
        InOut::<NodeOutIdx>::items_mut(self.kind.inputs_mut()).map(
            move |(ind, node_out_id)| {
                WithId::new(NodeInId::new(node_id, ind), node_out_id)
            },
        )
    }

    pub fn input_by_ind(&self, ind: usize) -> WithId<NodeInId, NodeOutId> {
        let module_id = self.module_id;
        WithId::new(
            NodeInId::new(self.node_id(), ind),
            NodeOutId::make(
                module_id,
                *InOut::<NodeOutIdx>::by_ind(self.kind.inputs(), ind),
            ),
        )
    }

    pub fn input_by_ind_mut(&mut self, ind: usize) -> WithId<NodeInId, &mut NodeOutIdx> {
        WithId::new(
            NodeInId::new(self.node_id(), ind),
            InOut::<NodeOutIdx>::by_ind_mut(self.kind.inputs_mut(), ind),
        )
    }

    pub fn inputs_len(&self) -> usize {
        InOut::<NodeOutIdx>::items_len(self.kind.inputs())
    }

    pub fn outputs(&self) -> impl Iterator<Item = WithId<NodeOutId, &NodeOutput>> + '_ {
        let node_id = self.node_id();
        InOut::<NodeOutput>::items(self.kind.outputs())
            .map(move |(ind, output)| WithId::new(NodeOutId::new(node_id, ind), output))
    }

    pub fn outputs_mut(
        &mut self,
    ) -> impl Iterator<Item = WithId<NodeOutId, &mut NodeOutput>> + '_ {
        let node_id = self.node_id();
        InOut::<NodeOutput>::items_mut(self.kind.outputs_mut())
            .map(move |(ind, output)| WithId::new(NodeOutId::new(node_id, ind), output))
    }

    pub fn output_by_ind(&self, ind: usize) -> WithId<NodeOutId, &NodeOutput> {
        WithId::new(
            NodeOutId::new(self.node_id(), ind),
            InOut::<NodeOutput>::by_ind(self.kind.outputs(), ind),
        )
    }

    pub fn output_by_ind_mut(
        &mut self,
        ind: usize,
    ) -> WithId<NodeOutId, &mut NodeOutput> {
        WithId::new(
            NodeOutId::new(self.node_id(), ind),
            InOut::<NodeOutput>::by_ind_mut(self.kind.outputs_mut(), ind),
        )
    }

    pub fn node_out_ids(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        let node_id = self.node_id();
        InOut::<NodeOutput>::items(self.kind.outputs())
            .map(move |(ind, _)| NodeOutId::new(node_id, ind))
    }

    pub fn outputs_len(&self) -> usize {
        InOut::<NodeOutput>::items_len(self.kind.outputs())
    }

    pub fn only_one_out(&self) -> WithId<NodeOutId, &NodeOutput> {
        if !self.is_temp_node() {
            assert_eq!(self.outputs_len(), 1);
        }
        self.output_by_ind(0)
    }

    pub fn only_one_out_mut(&mut self) -> WithId<NodeOutId, &mut NodeOutput> {
        assert_eq!(self.outputs_len(), 1);
        self.output_by_ind_mut(0)
    }

    pub fn assert(&self, net_list: &NetList) {
        self.kind.assert(self.module_id, net_list)
    }

    pub(crate) fn dump(&self, net_list: &NetList, prefix: &str, tab: &str) {
        println!(
            "{}{} (is_skip: {}, inject: {}, prev: {:?}, next: {:?})",
            prefix,
            self.kind.dump(),
            self.is_skip,
            self.inject,
            self.prev.map(|node_id| node_id.idx()),
            self.next.map(|node_id| node_id.idx())
        );

        match self.kind() {
            NodeKindWithId::ModInst(mod_inst) => {
                println!(
                    "{}mod_id = {} ({})",
                    tab,
                    mod_inst.module_id().idx(),
                    net_list[mod_inst.module_id()].name
                );
            }
            NodeKindWithId::Splitter(splitter) => {
                println!("{}start = {:?}", tab, splitter.start(),);
            }
            NodeKindWithId::Const(cons) => {
                if let (Some(value), Some(width)) =
                    (cons.value().opt_value(), cons.output().width().opt_value())
                {
                    println!("{}const = {}", tab, ConstVal::new(value, width));
                } else {
                    println!(
                        "{}const = {} (width = {})",
                        tab,
                        cons.value(),
                        cons.output().width()
                    );
                }
            }
            _ => {}
        }

        println!(
            "{}inputs: {}",
            tab,
            self.inputs()
                .map(|inp| format!("{} ({})", inp.node_id().idx(), inp.idx()))
                .intersperse(", ".to_string())
                .collect::<String>()
        );

        println!(
            "{}outputs: {}",
            tab,
            self.outputs()
                .map(|out| format!(
                    "{} ({})",
                    out.sym.map(|sym| sym.as_str()).unwrap_or("_"),
                    out.ty
                ))
                .intersperse(", ".to_string())
                .collect::<String>()
        );
    }

    pub fn is_input(&self) -> bool {
        matches!(&self.kind, NodeKind::Input(_))
    }

    pub fn is_expr(&self) -> bool {
        matches!(
            &self.kind,
            NodeKind::Not(_) | NodeKind::BitNot(_) | NodeKind::BinOp(_)
        )
    }

    pub fn is_const(&self) -> bool {
        matches!(&self.kind, NodeKind::Const(_) | NodeKind::MultiConst(_))
    }

    pub fn is_splitter(&self) -> bool {
        matches!(&self.kind, NodeKind::Splitter(_))
    }

    pub fn is_merger(&self) -> bool {
        matches!(&self.kind, NodeKind::Merger(_))
    }

    pub fn is_zero_extend(&self) -> bool {
        matches!(&self.kind, NodeKind::ZeroExtend(_))
    }

    pub fn is_mux(&self) -> bool {
        matches!(&self.kind, NodeKind::Mux2(_) | NodeKind::Case(_))
    }

    pub fn is_mod_inst(&self) -> bool {
        matches!(&self.kind, NodeKind::ModInst(_))
    }

    pub fn is_temp_node(&self) -> bool {
        matches!(&self.kind, NodeKind::TemplateNode(_))
    }
}

pub trait IsNode: Into<NodeKind> {
    type Inputs: InOut<NodeOutIdx> + ?Sized;
    type Outputs: InOut<NodeOutput> + ?Sized;

    fn inputs(&self) -> &Self::Inputs;

    fn inputs_mut(&mut self) -> &mut Self::Inputs;

    fn outputs(&self) -> &Self::Outputs;

    fn outputs_mut(&mut self) -> &mut Self::Outputs;

    fn assert(&self, _module_id: ModuleId, _net_list: &NetList) {}
}

macro_rules! define_nodes {
    (
        $( $kind:ident => $node:ident ),+ $(,)?
    ) => {
        #[derive(Debug, Clone, Encodable, Decodable)]
        pub enum NodeKind {
            $(
                $kind($node),
            )+
        }

        impl NodeKind {
            pub(crate) fn dump(&self) -> &'static str {
                match self {
                    $(
                        Self::$kind(_) => stringify!($kind),
                    )+
                }
            }

            pub(crate) fn assert(&self, module_id: ModuleId, net_list: &NetList) {
                match self {
                    $(
                        Self::$kind(node) => node.assert(module_id, net_list),
                    )+
                }

            }
        }

        // should be the same as Node
        #[derive(Debug)]
        pub enum NodeInputs {
            $(
                $kind($node),
            )+
        }

        impl InOut<NodeOutIdx> for NodeInputs {
            fn items_len(&self) -> usize {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutIdx>::items_len(node.inputs()),
                    )+
                }
            }

            #[auto_enum(Iterator)]
            fn items(&self) -> impl Iterator<Item = (usize, &NodeOutIdx)> + '_ {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutIdx>::items(node.inputs()),
                    )+
                }
            }

            #[auto_enum(Iterator)]
            fn items_mut(&mut self) -> impl Iterator<Item = (usize, &mut NodeOutIdx)> + '_ {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutIdx>::items_mut(node.inputs_mut()),
                    )+
                }
            }

            fn by_ind(&self, ind: usize) -> &NodeOutIdx {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutIdx>::by_ind(node.inputs(), ind),
                    )+
                }
            }

            fn by_ind_mut(&mut self, ind: usize) -> &mut NodeOutIdx {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutIdx>::by_ind_mut(node.inputs_mut(), ind),
                    )+
                }
            }

        }

        // should be the same as Node
        #[derive(Debug)]
        pub enum NodeOutputs {
            $(
                $kind($node),
            )+
        }

        impl InOut<NodeOutput> for NodeOutputs {
            fn items_len(&self) -> usize {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutput>::items_len(node.outputs()),
                    )+
                }
            }

            #[auto_enum(Iterator)]
            fn items(&self) -> impl Iterator<Item = (usize, &NodeOutput)> + '_ {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutput>::items(node.outputs()),
                    )+
                }
            }

            #[auto_enum(Iterator)]
            fn items_mut(&mut self) -> impl Iterator<Item = (usize, &mut NodeOutput)> + '_ {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutput>::items_mut(node.outputs_mut()),
                    )+
                }
            }

            fn by_ind(&self, ind: usize) -> &NodeOutput {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutput>::by_ind(node.outputs(), ind),
                    )+
                }
            }

            fn by_ind_mut(&mut self, ind: usize) -> &mut NodeOutput {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutput>::by_ind_mut(node.outputs_mut(), ind),
                    )+
                }
            }
        }

        impl IsNode for NodeKind {
            type Inputs = NodeInputs;
            type Outputs = NodeOutputs;

            fn inputs(&self) -> &NodeInputs {
                unsafe { mem::transmute(self) }
            }

            fn inputs_mut(&mut self) -> &mut NodeInputs {
                unsafe { mem::transmute(self) }
        }

            fn outputs(&self) -> &NodeOutputs {
                unsafe { mem::transmute(self) }
            }

            fn outputs_mut(&mut self) -> &mut NodeOutputs {
                unsafe { mem::transmute(self) }
            }
        }

        pub enum NodeKindWithId<'n> {
            $(
                $kind(WithId<ModuleId, &'n $node>),
            )+
        }

        impl<'n> NodeKindWithId<'n> {
            fn new(module_id: ModuleId, kind: &'n NodeKind) -> Self {
                match kind {
                    $(
                        NodeKind::$kind(kind) => Self::$kind(WithId::new(module_id, kind)),
                    )+
                }
            }

            pub fn clone(self) -> NodeKind {
                match self {
                    $(
                        Self::$kind(with_id) => NodeKind::$kind(with_id.into_inner().clone()),
                    )+
                }
            }
        }

        pub enum NodeKindWithIdMut<'n> {
            $(
                $kind(WithId<ModuleId, &'n mut $node>),
            )+
        }

        impl<'n> NodeKindWithIdMut<'n> {
            fn new(module_id: ModuleId, kind: &'n mut NodeKind) -> Self {
                match kind {
                    $(
                        NodeKind::$kind(kind) => Self::$kind(WithId::new(module_id, kind)),
                    )+
                }
            }
        }

        impl<R: Resolver> Resolve<R> for NodeKind {
            fn resolve(&self, resolver: &mut R) -> Result<Self, <R as Resolver>::Error> {
                Ok(match self {
                    $(
                        Self::$kind(kind) => Self::$kind(kind.resolve(resolver)?),
                    )+
                })
            }
        }

    };
}

impl !Sync for NodeKind {}
impl !Send for NodeKind {}

define_nodes!(
    Input => Input,
    ModInst => ModInst,
    Const => Const,
    MultiConst => MultiConst,
    Splitter => Splitter,
    Merger => Merger,
    ZeroExtend => ZeroExtend,
    Case => Case,
    BinOp => BinOpNode,
    BitNot => BitNot,
    Not => Not,
    Mux2 => Mux2,
    DFF => DFF,
    TemplateNode => TemplateNode,
);

impl NodeKind {}

macro_rules! assert_opt {
    ($lhs:expr, $rhs:expr) => {
        if let (Some(lhs), Some(rhs)) = ($lhs, $rhs) {
            assert_eq!(lhs, rhs);
        }
    };
}
use assert_opt;

macro_rules! assert_width {
    ($lhs:expr, $rhs:expr) => {
        crate::node::assert_opt!($lhs.opt_value(), $rhs.opt_value());
    };
}

use assert_width;
