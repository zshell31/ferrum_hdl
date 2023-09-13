mod bin_op;
mod bit_not;
mod bit_vec_trans;
mod consta;
mod dff;
mod expr;
mod input;
mod loop_nodes;
mod merger;
mod mod_inst;
mod mux2;
mod not;
mod pass;
mod splitter;

use std::mem;

use auto_enums::auto_enum;

pub use self::{
    bin_op::{BinOp, BinOpNode},
    bit_not::BitNotNode,
    bit_vec_trans::BitVecTrans,
    consta::ConstNode,
    dff::DFFNode,
    expr::Expr,
    input::InputNode,
    loop_nodes::{LoopEnd, LoopStart},
    merger::Merger,
    mod_inst::ModInst,
    mux2::Mux2Node,
    not::NotNode,
    pass::PassNode,
    splitter::Splitter,
};
use crate::{
    net_kind::NetKind,
    net_list::{NodeOutId, OutId},
    params::{Inputs, NodeOutWithId, NodeOutWithIdMut, Outputs},
    sig_ty::PrimTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct NodeOutput {
    pub ty: PrimTy,
    pub sym: Symbol,
    pub kind: NetKind,
}

pub trait IsNode: Into<Node> {
    type Inputs: Inputs + ?Sized;
    type Outputs: Outputs + ?Sized;

    fn inputs(&self) -> &Self::Inputs;

    fn outputs(&self) -> &Self::Outputs;

    fn outputs_mut(&mut self) -> &mut Self::Outputs;
}

macro_rules! define_nodes {
    (
        $( $kind:ident => $node:ident ),+ $(,)?
    ) => {
        #[derive(Debug)]
        pub enum Node {
            $(
                $kind($node),
            )+
        }

        // should be the same as Node
        #[derive(Debug)]
        pub enum NodeInputs {
            $(
                $kind($node),
            )+
        }

        impl Inputs for NodeInputs {
            #[auto_enum(Iterator)]
            fn items(&self) -> impl Iterator<Item = NodeOutId> + '_ {
                match self {
                    $(
                        Self::$kind(node) => Inputs::items(node.inputs()),
                    )+
                }
            }

            fn len(&self) -> usize {
                match self {
                    $(
                        Self::$kind(node) => Inputs::len(node.inputs()),
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

        impl Outputs for NodeOutputs {
            #[auto_enum(Iterator)]
            fn items(&self) -> impl Iterator<Item = NodeOutWithId<'_>> + '_ {
                match self {
                    $(
                        Self::$kind(node) => Outputs::items(node.outputs()),
                    )+
                }
            }

            #[auto_enum(Iterator)]
            fn items_mut(&mut self) -> impl Iterator<Item = NodeOutWithIdMut<'_>> + '_ {
                match self {
                    $(
                        Self::$kind(node) => Outputs::items_mut(node.outputs_mut()),
                    )+
                }
            }

            fn by_ind(&self, out: OutId) -> &NodeOutput {
                match self {
                    $(
                        Self::$kind(node) => Outputs::by_ind(node.outputs(), out),
                    )+
                }
            }

            fn by_ind_mut(&mut self, out: OutId) -> &mut NodeOutput {
                match self {
                    $(
                        Self::$kind(node) => Outputs::by_ind_mut(node.outputs_mut(), out),
                    )+
                }
            }

            fn len(&self) -> usize {
                match self {
                    $(
                        Self::$kind(node) => Outputs::len(node.outputs()),
                    )+
                }
            }

        }

        impl IsNode for Node {
            type Inputs = NodeInputs;
            type Outputs = NodeOutputs;

            fn inputs(&self) -> &NodeInputs {
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

impl !Sync for Node {}
impl !Send for Node {}

define_nodes!(
    DummyInput => InputNode,
    Input => InputNode,
    ModInst => ModInst,
    LoopStart => LoopStart,
    LoopEnd => LoopEnd,
    Expr => Expr,
    Pass => PassNode,
    Const => ConstNode,
    Splitter => Splitter,
    Merger => Merger,
    BitVecTrans => BitVecTrans,
    BinOp => BinOpNode,
    BitNot => BitNotNode,
    Not => NotNode,
    Mux2 => Mux2Node,
    DFF => DFFNode,
);

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

    pub fn is_pass(&self) -> bool {
        matches!(self, Self::Pass(_))
    }
}
