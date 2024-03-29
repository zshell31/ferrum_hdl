mod embedded_muxs;

use std::{
    fmt::Debug,
    iter::{self, Empty},
};

use either::Either;
use itertools::{Chunk, Chunks, IntoChunks, Itertools};
use smallvec::SmallVec;

use super::{IsNode, MakeNode, Merger, MergerArgs, Node, NodeKind, NodeOutput};
use crate::{
    const_val::ConstVal,
    cursor::Cursor,
    netlist::{Module, NodeId, Port, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mux {
    pub cases: SmallVec<[ConstVal; 2]>,
    pub inputs: u32,
    pub outputs: SmallVec<[NodeOutput; 1]>,
    pub default: Option<u32>,
}

impl Mux {
    #[inline]
    pub fn has_default(&self) -> bool {
        self.default.is_some()
    }

    pub fn case(&self, idx: usize) -> Case {
        let case = self.cases[idx];
        if case.is_zero_sized() {
            Case::Default
        } else {
            Case::Val(case)
        }
    }
}

#[derive(Debug)]
pub struct MuxArgs<V, O, D = Empty<Port>> {
    pub outputs: O,
    pub sel: Port,
    pub variants: V,
    pub default: Option<D>,
}

impl<V, I, O, D> MakeNode<MuxArgs<V, O, D>> for Mux
where
    I: IntoIterator<Item = Port>, // inputs for the each variant
    D: IntoIterator<Item = Port>, // inputs for the default variant
    V: IntoIterator<Item = (ConstVal, I)>, // variants
    O: IntoIterator<Item = (NodeTy, Option<Symbol>)>, // outputs
{
    fn make(module: &mut Module, args: MuxArgs<V, O, D>) -> NodeId {
        let sel_width = module[args.sel].width();
        assert!(sel_width != 0);

        let outputs = args
            .outputs
            .into_iter()
            .map(|(ty, sym)| NodeOutput::reg(ty, sym))
            .collect::<SmallVec<_>>();
        let outputs_len = outputs.len();

        let node_id = module.add_node(Mux {
            cases: SmallVec::new(),
            inputs: 0,
            outputs,
            default: None,
        });

        module.add_edge(args.sel, Port::new(node_id, 0));

        let mut inputs_len = 1;

        let variants = args.variants.into_iter();
        let mut cases = SmallVec::with_capacity(variants.size_hint().0);
        for (case, inputs) in variants {
            assert_eq!(case.width(), sel_width);
            cases.push(case);

            let mut idx = 0;
            for input in inputs {
                assert_eq!(
                    module[input].ty.width(),
                    module[node_id].outputs()[idx].ty.width()
                );
                module.add_edge(input, Port::new(node_id, inputs_len));

                inputs_len += 1;
                idx += 1;
            }
            assert_eq!(
                idx, outputs_len,
                "inputs len {idx} != outputs len {outputs_len}"
            );
        }

        let mut default = None;
        if let Some(inputs) = args.default {
            cases.push(ConstVal::default());

            let mut idx = 0;
            for input in inputs {
                assert_eq!(
                    module[input].ty.width(),
                    module[node_id].outputs()[idx].ty.width()
                );
                module.add_edge(input, Port::new(node_id, inputs_len));
                default.get_or_insert(inputs_len);

                inputs_len += 1;
                idx += 1;
            }
            assert_eq!(
                idx, outputs_len,
                "inputs len {idx} != outputs len {outputs_len}"
            );
        }

        assert!(!cases.is_empty());

        if let NodeKind::Mux(mux) = module[node_id].kind_mut() {
            mux.cases = cases;
            mux.inputs = inputs_len;
            mux.default = default;
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
        &self.outputs
    }

    #[inline]
    fn outputs_mut(&mut self) -> &mut [NodeOutput] {
        &mut self.outputs
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Case {
    Val(ConstVal),
    Default,
}

impl Case {
    pub fn val(self) -> ConstVal {
        match self {
            Self::Val(val) => val,
            Self::Default => ConstVal::default(),
        }
    }

    #[inline]
    pub fn is_default(&self) -> bool {
        matches!(self, Case::Default)
    }
}

pub struct Cases<C, P: Iterator> {
    cases: C,
    chunks: IntoChunks<P>,
}

impl<'c, C, P> IntoIterator for &'c Cases<C, P>
where
    C: Iterator + Clone,
    P: Iterator,
{
    type Item = (C::Item, Chunk<'c, P>);
    type IntoIter = CasesIter<'c, C, P>;

    fn into_iter(self) -> Self::IntoIter {
        CasesIter {
            cases: self.cases.clone(),
            chunks: self.chunks.into_iter(),
        }
    }
}

pub struct CasesIter<'c, C, P: Iterator> {
    cases: C,
    chunks: Chunks<'c, P>,
}

impl<'c, C, P> Iterator for CasesIter<'c, C, P>
where
    C: Iterator,
    P: Iterator,
{
    type Item = (C::Item, Chunk<'c, P>);

    fn next(&mut self) -> Option<Self::Item> {
        let case = self.cases.next()?;
        let chunk = self.chunks.next()?;

        Some((case, chunk))
    }
}

pub type PortAlias<'n> = impl Iterator<Item = Port> + 'n;
pub type CaseAlias<'n> = impl Iterator<Item = Case> + Clone + 'n;

pub struct MuxInputs<'n> {
    pub sel: Port,
    pub cases: Cases<CaseAlias<'n>, PortAlias<'n>>,
}

impl<'n> WithId<NodeId, &'n Mux> {
    pub fn inputs(self, module: &'n Module) -> MuxInputs<'n> {
        let cases = if self.has_default() {
            Either::Left(self.cases.iter().map(|case| {
                if !case.is_zero_sized() {
                    Case::Val(*case)
                } else {
                    Case::Default
                }
            }))
        } else {
            let len = self.cases.len();
            Either::Right(
                self.cases[.. len - 1]
                    .iter()
                    .map(|val| Case::Val(*val))
                    .chain(iter::once(Case::Default)),
            )
        };

        let mut inputs = module.incoming(self.id).into_iter_(module);
        let sel = inputs.next().unwrap();
        let chunks = inputs.chunks(self.outputs.len());

        MuxInputs {
            sel,
            cases: Cases { cases, chunks },
        }
    }

    pub fn sel(self, module: &Module) -> Port {
        let mut inputs = module.incoming(self.id);
        inputs.next_(module).unwrap()
    }

    pub fn default(self, module: &Module) -> impl Iterator<Item = Port> + '_ {
        let id = self.id;
        self.default
            .into_iter()
            .flat_map(move |default| module.incoming_iter(id).skip(default as usize))
    }

    fn has_the_same_default(self, other: WithId<NodeId, &Mux>, module: &Module) -> bool {
        self.out_count() == other.out_count()
            && self
                .default(module)
                .zip(other.default(module))
                .all(|(this, other)| this == other)
    }
}
