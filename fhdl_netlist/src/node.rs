mod bin_op;
mod bit_not;
mod case;
mod cons;
mod dff;
mod expr;
mod input;
mod loops;
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
    bit_not::BitNot,
    case::{BitVecMask, Case, CaseInputs},
    cons::Const,
    dff::{DFFInputs, DFF},
    expr::Expr,
    input::Input,
    loops::{LoopEnd, LoopStart},
    merger::Merger,
    mod_inst::ModInst,
    mux2::{Mux2, Mux2Inputs},
    not::Not,
    pass::{MultiPass, Pass},
    splitter::Splitter,
};
use crate::{
    arena::with_arena,
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
    // TODO: use flags
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
    pub kind: &'static mut NodeKind,
    // TODO: use flags
    pub is_skip: bool,
    pub inject: bool,
}

impl Node {
    pub fn new(kind: NodeKind) -> Self {
        let kind = unsafe { with_arena().alloc(kind) };
        Self::new_from_ref(kind)
    }

    pub fn new_from_ref(kind: &'static mut NodeKind) -> Self {
        Self {
            kind,
            is_skip: true,
            inject: false,
        }
    }
}

impl Clone for Node {
    fn clone(&self) -> Self {
        let kind = self.kind.clone();
        Self::new(kind)
    }
}

impl<T: Into<NodeKind>> From<T> for Node {
    fn from(node: T) -> Self {
        Self::new(node.into())
    }
}

impl From<&'static mut NodeKind> for Node {
    fn from(kind: &'static mut NodeKind) -> Self {
        Self::new_from_ref(kind)
    }
}

pub trait IsNode: Into<NodeKind> {
    type Inputs: Inputs + ?Sized;
    type Outputs: Outputs + ?Sized;

    fn inputs(&self) -> &Self::Inputs;

    fn inputs_mut(&mut self) -> &mut Self::Inputs;

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
        #[derive(Debug, Clone)]
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

            #[auto_enum(Iterator)]
            fn items_mut(&mut self) -> impl Iterator<Item = &mut NodeOutId> + '_ {
                match self {
                    $(
                        Self::$kind(node) => Inputs::items_mut(node.inputs_mut()),
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
    DummyInput => Input,
    Input => Input,
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
    BitNot => BitNot,
    Not => Not,
    Mux2 => Mux2,
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
        print_mem_size::<Input>();
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
        print_mem_size::<BitNot>();
        print_mem_size::<Not>();
        print_mem_size::<Mux2>();
        print_mem_size::<DFF>();
    }
}
