mod bin_op;
mod bit_not;
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

pub(crate) use self::cons::MultiConst;
pub use self::{
    bin_op::{BinOp, BinOpNode},
    bit_not::BitNotNode,
    case::{BitVecMask, Case},
    cons::Const,
    dff::{DFFInputs, DFF},
    expr::Expr,
    input::InputNode,
    loop_nodes::{LoopEnd, LoopStart},
    merger::Merger,
    mod_inst::ModInst,
    mux2::Mux2Node,
    not::NotNode,
    pass::{MultiPass, Pass},
    splitter::Splitter,
};
use crate::{
    net_kind::NetKind,
    net_list::{NodeId, NodeOutId, OutId},
    params::{Inputs, NodeOutWithId, NodeOutWithIdMut, Outputs},
    sig_ty::PrimTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, Copy)]
pub struct NodeOutput {
    pub ty: PrimTy,
    pub sym: Symbol,
    pub kind: NetKind,
    pub is_skip: bool,
    pub inject: bool,
}

impl NodeOutput {
    pub fn wire(ty: PrimTy, sym: Symbol) -> Self {
        Self::new(ty, sym, NetKind::Wire)
    }

    pub fn reg(ty: PrimTy, sym: Symbol) -> Self {
        Self::new(ty, sym, NetKind::Reg)
    }

    fn new(ty: PrimTy, sym: Symbol, kind: NetKind) -> Self {
        Self {
            ty,
            sym,
            kind,
            is_skip: true,
            inject: false,
        }
    }
}

impl NodeOutput {
    #[inline(always)]
    pub fn width(&self) -> u128 {
        self.ty.width()
    }
}

#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub is_skip: bool,
    pub inject: bool,
    pub from_const: bool,
}

impl<T: Into<NodeKind>> From<T> for Node {
    fn from(node: T) -> Self {
        let kind = node.into();
        let from_const = kind.is_const();

        Self {
            kind,
            is_skip: true,
            from_const,
            inject: false,
        }
    }
}

impl Node {
    #[inline(always)]
    pub fn is_input(&self) -> bool {
        self.kind.is_input()
    }

    #[inline(always)]
    pub fn is_dummy_input(&self) -> bool {
        self.kind.is_dummy_input()
    }

    #[inline(always)]
    pub fn is_pass(&self) -> bool {
        self.kind.is_pass()
    }

    #[inline(always)]
    pub fn is_expr(&self) -> bool {
        self.kind.is_expr()
    }

    #[inline(always)]
    pub fn is_const(&self) -> bool {
        self.kind.is_const()
    }

    #[inline(always)]
    pub fn is_splitter(&self) -> bool {
        self.kind.is_splitter()
    }

    #[inline(always)]
    pub fn is_merger(&self) -> bool {
        self.kind.is_merger()
    }

    #[inline(always)]
    pub fn is_mux(&self) -> bool {
        self.kind.is_mux()
    }

    #[inline(always)]
    pub fn is_mod_inst(&self) -> bool {
        self.kind.is_mod_inst()
    }

    #[inline(always)]
    pub fn inputs(&self) -> &<NodeKind as IsNode>::Inputs {
        self.kind.inputs()
    }

    #[inline(always)]
    pub fn outputs(&self) -> &<NodeKind as IsNode>::Outputs {
        self.kind.outputs()
    }

    #[inline(always)]
    pub fn outputs_mut(&mut self) -> &mut <NodeKind as IsNode>::Outputs {
        self.kind.outputs_mut()
    }

    #[inline(always)]
    pub fn node_out_ids(&self, node_id: NodeId) -> impl Iterator<Item = NodeOutId> + '_ {
        self.kind.node_out_ids(node_id)
    }
}

pub trait IsNode: Into<NodeKind> {
    type Inputs: Inputs + ?Sized;
    type Outputs: Outputs + ?Sized;

    fn inputs(&self) -> &Self::Inputs;

    fn outputs(&self) -> &Self::Outputs;

    fn outputs_mut(&mut self) -> &mut Self::Outputs;

    fn node_out_ids(&self, node_id: NodeId) -> impl Iterator<Item = NodeOutId> + '_ {
        self.outputs()
            .items()
            .map(move |output| output.node_out_id(node_id))
    }
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
    Pass => Pass,
    MultiPass => MultiPass,
    Const => Const,
    MultiConst => MultiConst,
    Splitter => Splitter,
    Merger => Merger,
    Case => Case,
    BinOp => BinOpNode,
    BitNot => BitNotNode,
    Not => NotNode,
    Mux2 => Mux2Node,
    DFF => DFF,
);

impl NodeKind {
    pub fn is_input(&self) -> bool {
        matches!(self, Self::Input(_))
    }

    pub fn is_dummy_input(&self) -> bool {
        matches!(self, Self::DummyInput(_))
    }

    pub fn is_pass(&self) -> bool {
        matches!(self, Self::Pass(_) | Self::MultiPass(_))
    }

    pub fn is_expr(&self) -> bool {
        matches!(self, Self::Not(_) | Self::BitNot(_) | Self::BinOp(_))
    }

    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const(_) | Self::MultiConst(_))
    }

    pub fn is_splitter(&self) -> bool {
        matches!(self, Self::Splitter(_))
    }

    pub fn is_merger(&self) -> bool {
        matches!(self, Self::Merger(_))
    }

    pub fn is_mux(&self) -> bool {
        matches!(self, Self::Mux2(_) | Self::Case(_))
    }

    pub fn is_mod_inst(&self) -> bool {
        matches!(self, Self::ModInst(_))
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
        print_mem_size::<Pass>();
        print_mem_size::<Const>();
        print_mem_size::<Splitter>();
        print_mem_size::<Merger>();
        print_mem_size::<Case>();
        print_mem_size::<BinOpNode>();
        print_mem_size::<BitNotNode>();
        print_mem_size::<NotNode>();
        print_mem_size::<Mux2Node>();
        print_mem_size::<DFF>();
    }
}
