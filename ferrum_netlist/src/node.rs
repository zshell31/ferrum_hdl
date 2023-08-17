use auto_enums::auto_enum;

pub use self::{
    bit_and::BitAndNode, bit_not::BitNotNode, bit_or::BitOrNode, consta::ConstNode,
    dff::DFFNode, input::InputNode, mux2::Mux2Node, not::NotNode, pass::PassNode,
};
use crate::{
    index::{Index, NodeIndex},
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

pub trait IsNode<I: Index>: Into<Node> {
    type Outputs: Outputs<I>;

    fn node_output(&self, out: u8) -> &NodeOutput;

    fn node_output_mut(&mut self, out: u8) -> &mut NodeOutput;

    fn inputs(&self) -> impl Iterator<Item = NodeIndex>;
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

    pub fn node_output<I: Index>(&self, out: u8) -> &NodeOutput {
        match self {
            Self::DummyInput(node) => IsNode::<I>::node_output(node, out),
            Self::Input(node) => IsNode::<I>::node_output(node, out),
            Self::Mux2(node) => IsNode::<I>::node_output(node, out),
            Self::Pass(node) => IsNode::<I>::node_output(node, out),
            Self::Const(node) => IsNode::<I>::node_output(node, out),
            Self::BitNot(node) => IsNode::<I>::node_output(node, out),
            Self::BitAnd(node) => IsNode::<I>::node_output(node, out),
            Self::BitOr(node) => IsNode::<I>::node_output(node, out),
            Self::Not(node) => IsNode::<I>::node_output(node, out),
            Self::DFF(node) => IsNode::<I>::node_output(node, out),
        }
    }

    pub fn node_output_mut<I: Index>(&mut self, out: u8) -> &mut NodeOutput {
        match self {
            Self::DummyInput(node) => IsNode::<I>::node_output_mut(node, out),
            Self::Input(node) => IsNode::<I>::node_output_mut(node, out),
            Self::Mux2(node) => IsNode::<I>::node_output_mut(node, out),
            Self::Pass(node) => IsNode::<I>::node_output_mut(node, out),
            Self::Const(node) => IsNode::<I>::node_output_mut(node, out),
            Self::BitNot(node) => IsNode::<I>::node_output_mut(node, out),
            Self::BitAnd(node) => IsNode::<I>::node_output_mut(node, out),
            Self::BitOr(node) => IsNode::<I>::node_output_mut(node, out),
            Self::Not(node) => IsNode::<I>::node_output_mut(node, out),
            Self::DFF(node) => IsNode::<I>::node_output_mut(node, out),
        }
    }

    #[auto_enum(Iterator)]
    pub fn inputs<I: Index>(&self) -> impl Iterator<Item = NodeIndex> {
        match self {
            Self::DummyInput(node) => IsNode::<I>::inputs(node),
            Self::Input(node) => IsNode::<I>::inputs(node),
            Self::Mux2(node) => IsNode::<I>::inputs(node),
            Self::Pass(node) => IsNode::<I>::inputs(node),
            Self::Const(node) => IsNode::<I>::inputs(node),
            Self::BitNot(node) => IsNode::<I>::inputs(node),
            Self::BitAnd(node) => IsNode::<I>::inputs(node),
            Self::BitOr(node) => IsNode::<I>::inputs(node),
            Self::Not(node) => IsNode::<I>::inputs(node),
            Self::DFF(node) => IsNode::<I>::inputs(node),
        }
    }
}
