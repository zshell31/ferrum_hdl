mod bin_op;
mod bit_not;
mod cons;
mod dff;
mod input;
mod memory;
mod merger;
mod mod_inst;
mod pass;
mod splitter;
mod switch;
mod zero_extend;

use std::{
    fmt::{self, Write},
    rc::Rc,
};

use fhdl_data_structures::{
    cursor::Cursor,
    graph::{Edges, GraphNode, IncomingDir, NodeId, OutgoingDir, Port},
    index::IndexType,
    list::{List, ListItem},
};

pub(crate) use self::cons::MultiConst;
pub use self::{
    bin_op::{BinOp, BinOpArgs, BinOpInputs, BinOpNode},
    bit_not::{BitNot, BitNotArgs},
    cons::{Const, ConstArgs},
    dff::{DFFArgs, DFFInputs, TyOrData, DFF},
    input::{GlSignalKind, Input, InputArgs},
    memory::{Memory, MemoryArgs},
    merger::{Merger, MergerArgs},
    mod_inst::{ModInst, ModInstArgs},
    pass::{Pass, PassArgs},
    splitter::{Indices, Splitter, SplitterArgs},
    switch::{Case, Switch, SwitchArgs, SwitchInputs, TupleCase},
    zero_extend::{Extend, ExtendArgs},
};
use crate::{
    netlist::{Module, NetList},
    node_ty::NodeTy,
    symbol::Symbol,
    with_id::WithId,
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
    incoming: List<Edges, IncomingDir>,
    outgoing: List<Edges, OutgoingDir>,
    kind: Box<NodeKind>,
    next: NodeId,
    prev: NodeId,
    span: Option<Rc<String>>,
}

impl GraphNode for Node {
    #[inline]
    fn incoming(&self) -> &List<Edges, IncomingDir> {
        &self.incoming
    }

    #[inline]
    fn outgoing(&self) -> &List<Edges, OutgoingDir> {
        &self.outgoing
    }

    #[inline]
    fn incoming_mut(&mut self) -> &mut List<Edges, IncomingDir> {
        &mut self.incoming
    }

    #[inline]
    fn outgoing_mut(&mut self) -> &mut List<Edges, OutgoingDir> {
        &mut self.outgoing
    }
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

    pub(crate) fn new_from(&self) -> Self {
        let mut node = Self::new(self.kind.as_ref().clone());
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
        matches!(&*self.kind, NodeKind::Switch(_))
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

    pub fn input(&self) -> Option<&Input> {
        match &*self.kind {
            NodeKind::Input(input) => Some(input),
            _ => None,
        }
    }

    pub fn input_mut(&mut self) -> Option<&mut Input> {
        match self.kind_mut() {
            NodeKind::Input(input) => Some(input),
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

    pub fn mux(&self) -> Option<&Switch> {
        match &*self.kind {
            NodeKind::Switch(mux) => Some(mux),
            _ => None,
        }
    }

    pub fn cons(&self) -> Option<&Const> {
        match &*self.kind {
            NodeKind::Const(cons) => Some(cons),
            _ => None,
        }
    }

    pub fn multi_cons(&self) -> Option<&MultiConst> {
        match &*self.kind {
            NodeKind::MultiConst(multi_const) => Some(multi_const),
            _ => None,
        }
    }

    pub fn memory(&self) -> Option<&Memory> {
        match &*self.kind {
            NodeKind::Memory(memory) => Some(memory),
            _ => None,
        }
    }

    pub fn memory_mut(&mut self) -> Option<&mut Memory> {
        match &mut *self.kind {
            NodeKind::Memory(memory) => Some(memory),
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
        buf: &mut impl Write,
        netlist: &NetList,
        module: &Module,
        prefix: &str,
        tab: &str,
    ) -> fmt::Result {
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

        writeln!(
            buf,
            "{}{} (skip: {}, prev: {}, next: {})",
            prefix,
            self.kind.dump(),
            self.skip,
            self.prev,
            self.next
        )?;

        let mut show_inputs = true;
        match &*self.kind {
            NodeKind::Input(input) => {
                writeln!(buf, "{}global = {:#?}", tab, input.global)?;
            }
            NodeKind::BinOp(bin_op) => {
                writeln!(buf, "{}op = {:?}", tab, bin_op.bin_op)?;
            }
            NodeKind::ModInst(mod_inst) => {
                writeln!(
                    buf,
                    "{}mod_id = {} ({})",
                    tab,
                    mod_inst.mod_id,
                    netlist[mod_inst.mod_id].borrow().name
                )?;
            }
            NodeKind::Splitter(splitter) => {
                writeln!(buf, "{}start = {}", tab, splitter.start.dump())?;
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
                writeln!(
                    buf,
                    "{}clk = {}, rst = {}, en = {}, init = {}, data = {}",
                    tab,
                    clk,
                    rst.dump(),
                    en.dump(),
                    init,
                    data
                )?;
            }
            NodeKind::Const(cons) => {
                show_inputs = false;
                writeln!(buf, "{}value = {}", tab, cons.value())?;
            }
            NodeKind::MultiConst(multi_cons) => {
                show_inputs = false;
                writeln!(
                    buf,
                    "{}values = [{}]",
                    tab,
                    multi_cons
                        .values()
                        .map(|value| value.to_string())
                        .intersperse(", ".to_string())
                        .collect::<String>()
                )?;
            }
            _ => {}
        }

        if show_inputs {
            writeln!(
                buf,
                "{}inputs:\n{}",
                tab,
                module
                    .incoming(self.id)
                    .into_iter_(module)
                    .map(|input| format!("{}{}{}", tab, tab, input))
                    .intersperse("\n".to_string())
                    .collect::<String>()
            )?;
        }

        writeln!(
            buf,
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
        )?;

        Ok(())
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
    Switch => Switch,
    Pass => Pass,
    Splitter => Splitter,
    Extend => Extend,
    Memory => Memory,
);
