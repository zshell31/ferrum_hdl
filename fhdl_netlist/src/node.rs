mod bin_op;
mod bit_not;
mod cons;
mod dff;
mod input;
mod merger;
mod mod_inst;
mod mux;
mod pass;
mod splitter;
mod zero_extend;

use std::rc::Rc;

pub(crate) use self::cons::MultiConst;
pub use self::{
    bin_op::{BinOp, BinOpArgs, BinOpInputs, BinOpNode},
    bit_not::{BitNot, BitNotArgs},
    cons::{Const, ConstArgs},
    dff::{DFFArgs, DFFInputs, TyOrData, DFF},
    input::{Input, InputArgs},
    merger::{Merger, MergerArgs},
    mod_inst::{ModInst, ModInstArgs},
    mux::{Case, Mux, MuxArgs, MuxInputs},
    pass::{Pass, PassArgs},
    splitter::{Indices, Splitter, SplitterArgs},
    zero_extend::{Extend, ExtendArgs},
};
use crate::{
    cursor::Cursor,
    netlist::{
        Edges, IncomingDir, IndexType, List, ListItem, Module, NetList, NodeId,
        OutgoingDir, Port, WithId,
    },
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NetKind {
    Wire,
    Reg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeOutput {
    pub ty: NodeTy,
    pub kind: NetKind,
    pub sym: Option<Symbol>,
    pub skip: bool,
}

impl NodeOutput {
    pub fn wire(ty: NodeTy, sym: Option<Symbol>) -> Self {
        Self::new(ty, NetKind::Wire, sym)
    }

    pub fn reg(ty: NodeTy, sym: Option<Symbol>) -> Self {
        Self::new(ty, NetKind::Reg, sym)
    }

    #[cfg(test)]
    pub fn set_skip(mut self, skip: bool) -> Self {
        self.skip = skip;
        self
    }

    fn new(ty: NodeTy, kind: NetKind, sym: Option<Symbol>) -> Self {
        Self {
            ty,
            kind,
            sym,
            skip: true,
        }
    }
}

impl NodeOutput {
    #[inline]
    pub fn width(&self) -> u128 {
        self.ty.width()
    }
}

#[derive(Debug)]
pub struct Node {
    pub skip: bool,
    pub(crate) incoming: List<Edges, IncomingDir>,
    pub(crate) outgoing: List<Edges, OutgoingDir>,
    kind: Box<NodeKind>,
    next: NodeId,
    prev: NodeId,
    span: Option<Rc<String>>,
}

impl ListItem<NodeId> for Node {
    #[inline]
    fn next(&self) -> NodeId {
        self.next
    }

    #[inline]
    fn set_next(&mut self, next: NodeId) {
        self.next = next;
    }

    #[inline]
    fn prev(&self) -> NodeId {
        self.prev
    }

    #[inline]
    fn set_prev(&mut self, prev: NodeId) {
        self.prev = prev;
    }
}

impl Node {
    pub fn new(kind: NodeKind) -> Self {
        Self {
            skip: true,
            kind: Box::new(kind),
            span: None,
            incoming: Default::default(),
            outgoing: Default::default(),
            next: NodeId::EMPTY,
            prev: NodeId::EMPTY,
        }
    }

    pub(crate) fn new_from(
        &self,
        calc_node_id: impl Fn(NodeId) -> NodeId + Copy,
    ) -> Self {
        let mut node = Self::new(self.kind.as_ref().clone());
        node.next = calc_node_id(self.next);
        node.prev = calc_node_id(self.prev);
        node.span.clone_from(&self.span);

        node
    }

    #[inline]
    pub fn kind(&self) -> &NodeKind {
        &self.kind
    }

    #[inline]
    fn kind_mut(&mut self) -> &mut NodeKind {
        &mut self.kind
    }

    pub fn set_span(&mut self, span: Option<String>) {
        self.span = span.map(Rc::new);
    }

    pub fn span(&self) -> Option<&str> {
        self.span.as_ref().map(|s| s.as_str())
    }

    pub(crate) fn span_rc(&self) -> Option<Rc<String>> {
        self.span.clone()
    }

    pub(crate) fn set_span_rc(&mut self, span: Option<Rc<String>>) {
        self.span = span;
    }

    pub fn is_input(&self) -> bool {
        matches!(&*self.kind, NodeKind::Input(_))
    }

    pub fn is_expr(&self) -> bool {
        matches!(&*self.kind, NodeKind::BitNot(_) | NodeKind::BinOp(_))
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
        matches!(&*self.kind, NodeKind::Extend(_))
    }

    pub fn is_mux(&self) -> bool {
        matches!(&*self.kind, NodeKind::Mux(_))
    }

    pub fn is_mod_inst(&self) -> bool {
        matches!(&*self.kind, NodeKind::ModInst(_))
    }

    pub fn mod_inst(&self) -> Option<&ModInst> {
        match &*self.kind {
            NodeKind::ModInst(mod_inst) => Some(mod_inst),
            _ => None,
        }
    }

    pub fn mod_inst_mut(&mut self) -> Option<&mut ModInst> {
        match self.kind_mut() {
            NodeKind::ModInst(mod_inst) => Some(mod_inst),
            _ => None,
        }
    }

    pub fn dff(&self) -> Option<&DFF> {
        match &*self.kind {
            NodeKind::DFF(dff) => Some(dff),
            _ => None,
        }
    }

    pub fn dff_mut(&mut self) -> Option<&mut DFF> {
        match self.kind_mut() {
            NodeKind::DFF(dff) => Some(dff),
            _ => None,
        }
    }

    pub fn mux(&self) -> Option<&Mux> {
        match &*self.kind {
            NodeKind::Mux(mux) => Some(mux),
            _ => None,
        }
    }
}

pub trait MakeNode<Args> {
    fn make(module: &mut Module, args: Args) -> NodeId;
}

pub trait IsNode {
    fn in_count(&self) -> usize;

    #[inline]
    fn out_count(&self) -> usize {
        self.outputs().len()
    }

    fn outputs(&self) -> &[NodeOutput];

    fn outputs_mut(&mut self) -> &mut [NodeOutput];
}

impl IsNode for Node {
    #[inline]
    fn in_count(&self) -> usize {
        self.kind.in_count()
    }

    #[inline]
    fn out_count(&self) -> usize {
        self.kind.out_count()
    }

    #[inline]
    fn outputs(&self) -> &[NodeOutput] {
        self.kind.outputs()
    }

    #[inline]
    fn outputs_mut(&mut self) -> &mut [NodeOutput] {
        self.kind_mut().outputs_mut()
    }
}

impl<'m, N: IsNode> WithId<NodeId, &'m N> {
    pub fn outputs(self) -> impl Iterator<Item = WithId<Port, &'m NodeOutput>> + 'm {
        let node_id = self.id;
        self.inner
            .outputs()
            .iter()
            .enumerate()
            .map(move |(idx, node_out)| {
                WithId::new(Port::new(node_id, idx as u32), node_out)
            })
    }

    pub fn out_ports(self) -> impl Iterator<Item = Port> {
        let out_count = self.out_count();
        let node_id = self.id;
        (0 .. out_count).map(move |port| Port::new(node_id, port as u32))
    }

    pub fn only_one_out(self) -> WithId<Port, &'m NodeOutput> {
        assert_eq!(self.out_count(), 1);
        WithId::new(Port::new(self.id, 0), &self.inner.outputs()[0])
    }
}

impl WithId<NodeId, &'_ Node> {
    pub(crate) fn dump(
        &self,
        netlist: &NetList,
        module: &Module,
        prefix: &str,
        tab: &str,
    ) {
        trait Dump {
            fn dump(&self) -> String;
        }

        impl<T: ToString> Dump for Option<T> {
            fn dump(&self) -> String {
                match self {
                    Some(value) => value.to_string(),
                    None => "_".to_string(),
                }
            }
        }

        println!(
            "{}{} (skip: {}, prev: {}, next: {})",
            prefix,
            self.kind.dump(),
            self.skip,
            self.prev,
            self.next
        );

        let mut show_inputs = true;
        match &*self.kind {
            NodeKind::BinOp(bin_op) => {
                println!("{}op = {:?}", tab, bin_op.bin_op);
            }
            NodeKind::ModInst(mod_inst) => {
                println!(
                    "{}mod_id = {} ({})",
                    tab,
                    mod_inst.mod_id,
                    netlist[mod_inst.mod_id].borrow().name
                );
            }
            NodeKind::Splitter(splitter) => {
                println!("{}start = {}", tab, splitter.start.dump());
            }
            NodeKind::DFF(dff) => {
                let DFFInputs {
                    clk,
                    rst,
                    en,
                    init,
                    data,
                } = self.with(dff).inputs(module);

                show_inputs = false;
                println!(
                    "{}clk = {}, rst = {}, en = {}, init = {}, data = {}",
                    tab,
                    clk,
                    rst.dump(),
                    en.dump(),
                    init,
                    data
                );
            }
            NodeKind::Const(cons) => {
                show_inputs = false;
                println!("{}value = {}", tab, cons.value());
            }
            NodeKind::MultiConst(multi_cons) => {
                show_inputs = false;
                println!(
                    "{}values = [{}]",
                    tab,
                    multi_cons
                        .values()
                        .map(|value| value.to_string())
                        .intersperse(", ".to_string())
                        .collect::<String>()
                );
            }
            _ => {}
        }

        if show_inputs {
            println!(
                "{}inputs:\n{}",
                tab,
                module
                    .incoming(self.id)
                    .into_iter_(module)
                    .map(|input| format!("{}{}{}", tab, tab, input))
                    .intersperse("\n".to_string())
                    .collect::<String>()
            );
        }

        println!(
            "{}outputs:\n{}",
            tab,
            self.outputs()
                .map(|out| format!(
                    "{}{}{} ({} skip: {}){}",
                    tab,
                    tab,
                    out.sym.map(|sym| sym.as_str()).unwrap_or("_"),
                    out.ty,
                    out.skip,
                    if module.is_mod_output(out.id) {
                        " MOD_OUT"
                    } else {
                        ""
                    }
                ))
                .intersperse("\n".to_string())
                .collect::<String>()
        );
    }
}

macro_rules! define_nodes {
    (
        $( $kind:ident => $node:ident ),+ $(,)?
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum NodeKind {
            $(
                $kind($node),
            )+
        }

        $(
            impl From<$node> for NodeKind {
                fn from(kind: $node) -> Self {
                    Self::$kind(kind)
                }
            }
        )+

        impl NodeKind {
            pub fn in_count(&self) -> usize {
                match self {
                    $(
                        Self::$kind(kind) => kind.in_count(),
                    )+
                }
            }

            pub fn out_count(&self) -> usize {
                match self {
                    $(
                        Self::$kind(kind) => kind.out_count(),
                    )+
                }
            }

            pub fn outputs(&self) -> &[NodeOutput] {
                match self {
                    $(
                        Self::$kind(kind) => kind.outputs(),
                    )+
                }
            }

            pub fn outputs_mut(&mut self) -> &mut [NodeOutput] {
                match self {
                    $(
                        Self::$kind(kind) => kind.outputs_mut(),
                    )+
                }
            }

            pub(crate) fn dump(&self) -> &'static str {
                match self {
                    $(
                        Self::$kind(_) => stringify!($kind),
                    )+
                }
            }
        }
    };
}

define_nodes!(
    BinOp => BinOpNode,
    BitNot => BitNot,
    Const => Const,
    DFF => DFF,
    Input => Input,
    Merger => Merger,
    ModInst => ModInst,
    MultiConst => MultiConst,
    Mux => Mux,
    Pass => Pass,
    Splitter => Splitter,
    Extend => Extend,
);
