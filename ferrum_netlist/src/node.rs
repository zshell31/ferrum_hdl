use auto_enums::auto_enum;

pub use self::{
    bin_op::{BinOp, BinOpNode},
    bit_not::BitNotNode,
    consta::ConstNode,
    dff::DFFNode,
    input::InputNode,
    mod_inst::ModInst,
    mux2::Mux2Node,
    not::NotNode,
    pass::PassNode,
    splitter::Splitter,
};
use crate::{
    net_list::NodeId,
    output::{NodeOutput, Outputs},
};

mod bin_op;
mod bit_not;
mod consta;
mod dff;
mod input;
mod mod_inst;
mod mux2;
mod not;
mod pass;
mod splitter;

pub trait IsNode: Into<Node> {
    type Outputs: Outputs;

    fn node_output(&self, out: u8) -> &NodeOutput;

    fn node_output_mut(&mut self, out: u8) -> &mut NodeOutput;

    fn inputs(&self) -> impl Iterator<Item = NodeId> + '_;
}

#[derive(Debug, Clone)]
pub enum Node {
    DummyInput(InputNode),
    Input(InputNode),
    ModInst(ModInst),
    Pass(PassNode),
    Const(ConstNode),
    Splitter(Splitter),
    BinOp(BinOpNode),
    BitNot(BitNotNode),
    Not(NotNode),
    Mux2(Mux2Node),
    DFF(DFFNode),
}

impl Node {
    pub fn is_input(&self) -> bool {
        matches!(self, Self::Input(_))
    }

    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const(_))
    }

    pub fn is_dummy_input(&self) -> bool {
        matches!(self, Self::DummyInput(_))
    }

    pub fn node_output(&self, out: u8) -> &NodeOutput {
        match self {
            Self::DummyInput(node) => node.node_output(out),
            Self::Input(node) => node.node_output(out),
            Self::ModInst(node) => node.node_output(out),
            Self::Mux2(node) => node.node_output(out),
            Self::Pass(node) => node.node_output(out),
            Self::Const(node) => node.node_output(out),
            Self::Splitter(node) => node.node_output(out),
            Self::BinOp(node) => node.node_output(out),
            Self::BitNot(node) => node.node_output(out),
            Self::Not(node) => node.node_output(out),
            Self::DFF(node) => node.node_output(out),
        }
    }

    pub fn node_output_mut(&mut self, out: u8) -> &mut NodeOutput {
        match self {
            Self::DummyInput(node) => node.node_output_mut(out),
            Self::Input(node) => node.node_output_mut(out),
            Self::ModInst(node) => node.node_output_mut(out),
            Self::Mux2(node) => node.node_output_mut(out),
            Self::Pass(node) => node.node_output_mut(out),
            Self::Const(node) => node.node_output_mut(out),
            Self::Splitter(node) => node.node_output_mut(out),
            Self::BinOp(node) => node.node_output_mut(out),
            Self::BitNot(node) => node.node_output_mut(out),
            Self::Not(node) => node.node_output_mut(out),
            Self::DFF(node) => node.node_output_mut(out),
        }
    }

    #[auto_enum(Iterator)]
    pub fn inputs(&self) -> impl Iterator<Item = NodeId> + '_ {
        match self {
            Self::DummyInput(node) => node.inputs(),
            Self::Input(node) => node.inputs(),
            Self::ModInst(node) => node.inputs(),
            Self::Mux2(node) => node.inputs(),
            Self::Pass(node) => node.inputs(),
            Self::Const(node) => node.inputs(),
            Self::Splitter(node) => node.inputs(),
            Self::BinOp(node) => node.inputs(),
            Self::BitNot(node) => node.inputs(),
            Self::Not(node) => node.inputs(),
            Self::DFF(node) => node.inputs(),
        }
    }
}
