use auto_enums::auto_enum;

pub use self::{
    bit_and::BitAndNode, bit_not::BitNotNode, bit_or::BitOrNode, consta::ConstNode,
    dff::DFFNode, input::InputNode, mux2::Mux2Node, not::NotNode, pass::PassNode,
};
use crate::{
    net_list::NodeId,
    output::{NodeOutput, Outputs},
};

mod bit_and;
mod bit_not;
mod bit_or;
mod consta;
mod dff;
mod input;
mod mux2;
mod not;
mod pass;

pub trait IsNode: Into<Node> {
    type Outputs: Outputs;

    fn node_output(&self, out: u8) -> &NodeOutput;

    fn node_output_mut(&mut self, out: u8) -> &mut NodeOutput;

    fn inputs(&self) -> impl Iterator<Item = NodeId>;
}

#[derive(Debug, Clone, Copy)]
pub enum Node {
    DummyInput(InputNode),
    Input(InputNode),
    Pass(PassNode),
    Const(ConstNode),
    BitNot(BitNotNode),
    BitAnd(BitAndNode),
    BitOr(BitOrNode),
    Not(NotNode),
    Mux2(Mux2Node),
    DFF(DFFNode),
}

impl Node {
    pub fn is_input(&self) -> bool {
        matches!(self, Self::Input(_))
    }

    pub fn is_dummy_input(&self) -> bool {
        matches!(self, Self::DummyInput(_))
    }

    pub fn node_output(&self, out: u8) -> &NodeOutput {
        match self {
            Self::DummyInput(node) => node.node_output(out),
            Self::Input(node) => node.node_output(out),
            Self::Mux2(node) => node.node_output(out),
            Self::Pass(node) => node.node_output(out),
            Self::Const(node) => node.node_output(out),
            Self::BitNot(node) => node.node_output(out),
            Self::BitAnd(node) => node.node_output(out),
            Self::BitOr(node) => node.node_output(out),
            Self::Not(node) => node.node_output(out),
            Self::DFF(node) => node.node_output(out),
        }
    }

    pub fn node_output_mut(&mut self, out: u8) -> &mut NodeOutput {
        match self {
            Self::DummyInput(node) => node.node_output_mut(out),
            Self::Input(node) => node.node_output_mut(out),
            Self::Mux2(node) => node.node_output_mut(out),
            Self::Pass(node) => node.node_output_mut(out),
            Self::Const(node) => node.node_output_mut(out),
            Self::BitNot(node) => node.node_output_mut(out),
            Self::BitAnd(node) => node.node_output_mut(out),
            Self::BitOr(node) => node.node_output_mut(out),
            Self::Not(node) => node.node_output_mut(out),
            Self::DFF(node) => node.node_output_mut(out),
        }
    }

    #[auto_enum(Iterator)]
    pub fn inputs(&self) -> impl Iterator<Item = NodeId> {
        match self {
            Self::DummyInput(node) => node.inputs(),
            Self::Input(node) => node.inputs(),
            Self::Mux2(node) => node.inputs(),
            Self::Pass(node) => node.inputs(),
            Self::Const(node) => node.inputs(),
            Self::BitNot(node) => node.inputs(),
            Self::BitAnd(node) => node.inputs(),
            Self::BitOr(node) => node.inputs(),
            Self::Not(node) => node.inputs(),
            Self::DFF(node) => node.inputs(),
        }
    }
}
