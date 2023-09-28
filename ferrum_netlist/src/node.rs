mod bin_op;
mod bit_not;
mod bit_vec_trans;
mod case;
mod cons;
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
    case::{BitVecMask, Case},
    cons::ConstNode,
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

pub trait IsNode: Into<NodeKind> {
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
        pub enum NodeKind {
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

            fn by_ind(&self, ind: OutId) -> NodeOutWithId<'_> {
                match self {
                    $(
                        Self::$kind(node) => Outputs::by_ind(node.outputs(), ind),
                    )+
                }
            }

            fn by_ind_mut(&mut self, ind: OutId) -> NodeOutWithIdMut<'_> {
                match self {
                    $(
                        Self::$kind(node) => Outputs::by_ind_mut(node.outputs_mut(), ind),
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

        impl IsNode for NodeKind {
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

impl !Sync for NodeKind {}
impl !Send for NodeKind {}

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
    Case => Case,
    BitVecTrans => BitVecTrans,
    BinOp => BinOpNode,
    BitNot => BitNotNode,
    Not => NotNode,
    Mux2 => Mux2Node,
    DFF => DFFNode,
);

impl NodeKind {
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

#[cfg(test)]
mod tests {
    use super::*;

    fn print_mem_size<T>() {
        use std::any;
        println!("{} {}", any::type_name::<T>(), mem::size_of::<T>());
    }

    #[test]
    fn maxsize() {
        print_mem_size::<NodeKind>();
        print_mem_size::<InputNode>();
        print_mem_size::<ModInst>();
        print_mem_size::<LoopStart>();
        print_mem_size::<LoopEnd>();
        print_mem_size::<Expr>();
        print_mem_size::<PassNode>();
        print_mem_size::<ConstNode>();
        print_mem_size::<Splitter>();
        print_mem_size::<Merger>();
        print_mem_size::<Case>();
        print_mem_size::<BitVecTrans>();
        print_mem_size::<BinOpNode>();
        print_mem_size::<BitNotNode>();
        print_mem_size::<NotNode>();
        print_mem_size::<Mux2Node>();
        print_mem_size::<DFFNode>();
    }
}
