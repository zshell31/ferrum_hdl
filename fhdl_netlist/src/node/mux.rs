use std::{fmt::Debug, iter};

use either::Either;
use smallvec::SmallVec;

use super::{IsNode, MakeNode, NodeKind, NodeOutput};
use crate::{
    const_val::ConstVal,
    netlist::{Cursor, CursorMut, Module, NodeId, Port, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mux {
    pub cases: SmallVec<[ConstVal; 2]>,
    pub inputs: u32,
    pub output: [NodeOutput; 1],
    pub has_default: bool,
}

#[derive(Debug)]
pub struct MuxArgs<V> {
    pub ty: NodeTy,
    pub sel: Port,
    pub variants: V,
    pub default: Option<Port>,
    pub sym: Option<Symbol>,
}

impl<V> MakeNode<MuxArgs<V>> for Mux
where
    V: CursorMut<Item = (ConstVal, Port)>,
{
    fn make(module: &mut Module, mut args: MuxArgs<V>) -> NodeId {
        let sel_width = module[args.sel].width();
        assert!(sel_width != 0);

        let node_id = module.add_node(Mux {
            cases: SmallVec::new(),
            inputs: 0,
            output: [NodeOutput::reg(args.ty, args.sym)],
            has_default: args.default.is_some(),
        });

        module.add_edge(args.sel, Port::new(node_id, 0));

        let mut inputs = 1;
        let mut cases = SmallVec::with_capacity(args.variants.size());
        while let Some((case, input)) = args.variants.next(module) {
            assert_eq!(case.width(), sel_width);
            assert_eq!(module[input].ty, args.ty);

            cases.push(case);
            module.add_edge(input, Port::new(node_id, inputs));
            inputs += 1;
        }

        if let Some(default) = args.default {
            assert_eq!(module[default].ty, args.ty);

            cases.push(ConstVal::default());
            module.add_edge(default, Port::new(node_id, inputs));
            inputs += 1;
        }

        assert!(!cases.is_empty());

        if let NodeKind::Mux(mux) = &mut *module[node_id].kind {
            mux.cases = cases;
            mux.inputs = inputs;
        }

        node_id
    }
}

impl IsNode for Mux {
    #[inline]
    fn in_count(&self) -> usize {
        self.inputs as usize
    }

    #[inline]
    fn outputs(&self) -> &[NodeOutput] {
        &self.output
    }

    #[inline]
    fn outputs_mut(&mut self) -> &mut [NodeOutput] {
        &mut self.output
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Case {
    Val(ConstVal),
    Default,
}

pub type Cases<'n> = impl Iterator<Item = (Case, Port)> + 'n;

pub struct MuxInputs<'n> {
    pub sel: Port,
    pub cases: Cases<'n>,
}

impl WithId<NodeId, &'_ Mux> {
    pub fn inputs<'n>(&'n self, module: &'n Module) -> MuxInputs<'n> {
        let cases = if self.has_default {
            Either::Left(self.cases.iter().map(|case| {
                if !case.is_zero_sized() {
                    Case::Val(*case)
                } else {
                    Case::Default
                }
            }))
        } else {
            let cases = self.cases.len();
            Either::Right(
                self.cases[.. cases - 1]
                    .iter()
                    .map(|case| Case::Val(*case))
                    .chain(iter::once(Case::Default)),
            )
        };

        let mut inputs = module.incoming(self.id).into_iter(module);
        let sel = Iterator::next(&mut inputs).unwrap();

        MuxInputs {
            sel,
            cases: cases.zip(inputs),
        }
    }
}
