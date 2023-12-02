mod bin_op;
mod bit_not;
mod case;
mod cons;
mod dff;
mod gen_node;
mod input;
mod merger;
mod mod_inst;
mod mux2;
mod not;
mod splitter;
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
    gen_node::GenNode,
    input::Input,
    merger::Merger,
    mod_inst::ModInst,
    mux2::{Mux2, Mux2Inputs},
    not::Not,
    splitter::Splitter,
    zero_extend::ZeroExtend,
};
use crate::{
    const_val::ConstVal,
    net_list::{InOut, NetList, NodeId, NodeInId, NodeOutId, WithId},
    sig_ty::{ConstParam, NodeTy},
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
    // TODO: use flags
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
    #[inline(always)]
    pub fn width(&self) -> ConstParam {
        self.ty.width()
    }
}

#[derive(Debug, Clone, Encodable, Decodable)]
pub struct Node {
    pub kind: Box<NodeKind>,
    pub is_skip: bool,
    pub inject: bool,
    node_id: NodeId,
    next: Option<NodeId>,
    prev: Option<NodeId>,
}

impl Node {
    pub fn new(node_id: NodeId, kind: NodeKind) -> Self {
        Self {
            kind: Box::new(kind),
            is_skip: true,
            inject: false,
            node_id,
            next: None,
            prev: None,
        }
    }

    pub fn next(&self) -> Option<NodeId> {
        self.next
    }

    pub fn set_next(&mut self, node_id: Option<NodeId>) {
        self.next = node_id;
    }

    pub fn prev(&self) -> Option<NodeId> {
        self.prev
    }

    pub fn set_prev(&mut self, node_id: Option<NodeId>) {
        self.prev = node_id;
    }

    pub fn node_id(&self) -> NodeId {
        self.node_id
    }

    pub fn inputs(&self) -> impl Iterator<Item = WithId<NodeInId, NodeOutId>> + '_ {
        let node_id = self.node_id;
        InOut::<NodeOutId>::items(self.kind.inputs()).map(move |(ind, node_out_id)| {
            WithId::new(NodeInId::new(node_id, ind), *node_out_id)
        })
    }

    pub fn inputs_mut(
        &mut self,
    ) -> impl Iterator<Item = WithId<NodeInId, &mut NodeOutId>> + '_ {
        let node_id = self.node_id;
        InOut::<NodeOutId>::items_mut(self.kind.inputs_mut()).map(
            move |(ind, node_out_id)| {
                WithId::new(NodeInId::new(node_id, ind), node_out_id)
            },
        )
    }

    pub fn input_by_ind(&self, ind: usize) -> WithId<NodeInId, NodeOutId> {
        WithId::new(
            NodeInId::new(self.node_id, ind),
            *InOut::<NodeOutId>::by_ind(self.kind.inputs(), ind),
        )
    }

    pub fn input_by_ind_mut(&mut self, ind: usize) -> WithId<NodeInId, &mut NodeOutId> {
        WithId::new(
            NodeInId::new(self.node_id, ind),
            InOut::<NodeOutId>::by_ind_mut(self.kind.inputs_mut(), ind),
        )
    }

    pub fn inputs_len(&self) -> usize {
        InOut::<NodeOutId>::items_len(self.kind.inputs())
    }

    pub fn outputs(&self) -> impl Iterator<Item = WithId<NodeOutId, &NodeOutput>> + '_ {
        let node_id = self.node_id;
        InOut::<NodeOutput>::items(self.kind.outputs())
            .map(move |(ind, output)| WithId::new(NodeOutId::new(node_id, ind), output))
    }

    pub fn outputs_mut(
        &mut self,
    ) -> impl Iterator<Item = WithId<NodeOutId, &mut NodeOutput>> + '_ {
        let node_id = self.node_id;
        InOut::<NodeOutput>::items_mut(self.kind.outputs_mut())
            .map(move |(ind, output)| WithId::new(NodeOutId::new(node_id, ind), output))
    }

    pub fn output_by_ind(&self, ind: usize) -> WithId<NodeOutId, &NodeOutput> {
        WithId::new(
            NodeOutId::new(self.node_id, ind),
            InOut::<NodeOutput>::by_ind(self.kind.outputs(), ind),
        )
    }

    pub fn output_by_ind_mut(
        &mut self,
        ind: usize,
    ) -> WithId<NodeOutId, &mut NodeOutput> {
        WithId::new(
            NodeOutId::new(self.node_id, ind),
            InOut::<NodeOutput>::by_ind_mut(self.kind.outputs_mut(), ind),
        )
    }

    pub fn node_out_ids(&self) -> impl Iterator<Item = NodeOutId> + '_ {
        let node_id = self.node_id;
        InOut::<NodeOutput>::items(self.kind.outputs())
            .map(move |(ind, _)| NodeOutId::new(node_id, ind))
    }

    pub fn outputs_len(&self) -> usize {
        InOut::<NodeOutput>::items_len(self.kind.outputs())
    }

    pub fn only_one_out(&self) -> WithId<NodeOutId, &NodeOutput> {
        assert_eq!(self.outputs_len(), 1);
        self.output_by_ind(0)
    }

    pub fn only_one_out_mut(&mut self) -> WithId<NodeOutId, &mut NodeOutput> {
        assert_eq!(self.outputs_len(), 1);
        self.output_by_ind_mut(0)
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

        match &*self.kind {
            NodeKind::ModInst(mod_inst) => {
                println!(
                    "{}mod_id = {} ({})",
                    tab,
                    mod_inst.module_id.idx(),
                    net_list[mod_inst.module_id].name
                );
            }
            NodeKind::Splitter(splitter) => {
                println!(
                    "{}start = {} rev = {}",
                    tab,
                    splitter.start(net_list),
                    splitter.rev
                );
            }
            NodeKind::Const(cons) => {
                if let (Some(value), Some(width)) =
                    (cons.value.opt_value(), cons.output.width().opt_value())
                {
                    println!("{}const = {}", tab, ConstVal::new(value, width));
                } else {
                    println!(
                        "{}const = {} (width = {})",
                        tab,
                        cons.value,
                        cons.output.width()
                    );
                }
            }
            _ => {}
        }

        println!(
            "{}inputs: {}",
            tab,
            self.inputs()
                .map(|inp| format!("{} ({})", inp.node_id().idx(), inp.out_id()))
                .intersperse(", ".to_string())
                .collect::<String>()
        );

        println!(
            "{}outputs: {}",
            tab,
            self.outputs()
                .map(|out| format!(
                    "{} ({:?})",
                    out.sym.map(|sym| sym.as_str()).unwrap_or("_"),
                    out.ty
                ))
                .intersperse(", ".to_string())
                .collect::<String>()
        );
    }

    pub fn is_input(&self) -> bool {
        matches!(&*self.kind, NodeKind::Input(_))
    }

    pub fn is_expr(&self) -> bool {
        matches!(
            &*self.kind,
            NodeKind::Not(_) | NodeKind::BitNot(_) | NodeKind::BinOp(_)
        )
    }

    pub fn is_const(&self) -> bool {
        matches!(&*self.kind, NodeKind::Const(_) | NodeKind::MultiConst(_))
    }

    pub fn is_splitter(&self) -> bool {
        matches!(&*self.kind, NodeKind::Splitter(_))
    }

    pub fn is_merger(&self) -> bool {
        matches!(&*self.kind, NodeKind::Merger(_))
    }

    pub fn is_zero_extend(&self) -> bool {
        matches!(&*self.kind, NodeKind::ZeroExtend(_))
    }

    pub fn is_mux(&self) -> bool {
        matches!(&*self.kind, NodeKind::Mux2(_) | NodeKind::Case(_))
    }

    pub fn is_mod_inst(&self) -> bool {
        matches!(&*self.kind, NodeKind::ModInst(_))
    }
}

pub trait IsNode: Into<NodeKind> {
    type Inputs: InOut<NodeOutId> + ?Sized;
    type Outputs: InOut<NodeOutput> + ?Sized;

    fn inputs(&self) -> &Self::Inputs;

    fn inputs_mut(&mut self) -> &mut Self::Inputs;

    fn outputs(&self) -> &Self::Outputs;

    fn outputs_mut(&mut self) -> &mut Self::Outputs;

    fn validate(&self, _net_list: &NetList) {}
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
        }

        // should be the same as Node
        #[derive(Debug)]
        pub enum NodeInputs {
            $(
                $kind($node),
            )+
        }

        impl InOut<NodeOutId> for NodeInputs {
            fn items_len(&self) -> usize {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutId>::items_len(node.inputs()),
                    )+
                }
            }

            #[auto_enum(Iterator)]
            fn items(&self) -> impl Iterator<Item = (usize, &NodeOutId)> + '_ {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutId>::items(node.inputs()),
                    )+
                }
            }

            #[auto_enum(Iterator)]
            fn items_mut(&mut self) -> impl Iterator<Item = (usize, &mut NodeOutId)> + '_ {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutId>::items_mut(node.inputs_mut()),
                    )+
                }
            }

            fn by_ind(&self, ind: usize) -> &NodeOutId {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutId>::by_ind(node.inputs(), ind),
                    )+
                }
            }

            fn by_ind_mut(&mut self, ind: usize) -> &mut NodeOutId {
                match self {
                    $(
                        Self::$kind(node) => InOut::<NodeOutId>::by_ind_mut(node.inputs_mut(), ind),
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
    GenNode => GenNode,
);

impl NodeKind {}
