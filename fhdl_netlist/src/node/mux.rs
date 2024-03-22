use std::{
    fmt::Debug,
    iter::{self, Empty},
};

use either::Either;
use smallvec::SmallVec;

use super::{IsNode, MakeNode, NodeKind, NodeOutput};
use crate::{
    const_val::ConstVal,
    netlist::{Chunk, Chunks, Cursor, CursorMut, Module, NodeId, Port, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mux {
    pub cases: SmallVec<[ConstVal; 2]>,
    pub inputs: u32,
    pub outputs: SmallVec<[NodeOutput; 1]>,
    pub has_default: bool,
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
    I: CursorMut<Item = Port, Storage = Module>, // inputs for the each variant
    D: CursorMut<Item = Port, Storage = Module>, // inputs for the default variant
    V: CursorMut<Item = (ConstVal, I), Storage = Module>, // variants
    O: CursorMut<Item = (NodeTy, Option<Symbol>), Storage = Module>, // outputs
{
    fn make(module: &mut Module, mut args: MuxArgs<V, O, D>) -> NodeId {
        let sel_width = module[args.sel].width();
        assert!(sel_width != 0);

        let outputs = args
            .outputs
            .into_iter_mut(module)
            .map(|(ty, sym)| NodeOutput::reg(ty, sym))
            .collect::<SmallVec<_>>();
        let outputs_len = outputs.len();

        let node_id = module.add_node(Mux {
            cases: SmallVec::new(),
            inputs: 0,
            outputs,
            has_default: args.default.is_some(),
        });

        module.add_edge(args.sel, Port::new(node_id, 0));

        let mut inputs_len = 1;
        let mut cases = SmallVec::with_capacity(args.variants.size());
        while let Some((case, mut inputs)) = args.variants.next_mut(module) {
            assert_eq!(case.width(), sel_width);
            cases.push(case);

            let mut idx = 0;
            while let Some(input) = inputs.next_mut(module) {
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

        if let Some(mut default) = args.default {
            cases.push(ConstVal::default());

            let mut idx = 0;
            while let Some(input) = default.next_mut(module) {
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

        assert!(!cases.is_empty());

        if let NodeKind::Mux(mux) = &mut *module[node_id].kind {
            mux.cases = cases;
            mux.inputs = inputs_len;
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

pub struct Cases<C, P> {
    cases: C,
    chunks: Chunks<P>,
}

impl<C, P> Cases<C, P>
where
    C: Iterator<Item = Case>,
    P: Cursor<Item = Port, Storage = Module>,
{
    pub fn next(&mut self) -> Option<(Case, Chunk<'_, P>)> {
        let case = self.cases.next()?;
        let chunk = self.chunks.next_chunk()?;

        Some((case, chunk))
    }
    pub fn next_case(&mut self, module: &Module) -> Option<Case> {
        let case = self.cases.next()?;
        self.chunks.skip_chunk(module)?;

        Some(case)
    }

    #[inline]
    pub fn into_chunks(self) -> Chunks<P> {
        self.chunks
    }
}

pub type PortAlias = impl Cursor<Item = Port, Storage = Module>;
pub type CaseAlias<'n> = impl Iterator<Item = Case> + 'n;

pub struct MuxInputs<'n> {
    pub sel: Port,
    pub cases: Cases<CaseAlias<'n>, PortAlias>,
}

impl WithId<NodeId, &'_ Mux> {
    pub fn inputs<'n>(&'n self, module: &Module) -> MuxInputs<'n> {
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

        let mut inputs = module.incoming(self.id);
        let sel = inputs.next_(module).unwrap();

        let chunks = Chunks::new(inputs, (self.inputs - 1) as usize, self.outputs.len());

        MuxInputs {
            sel,
            cases: Cases { cases, chunks },
        }
    }
}
