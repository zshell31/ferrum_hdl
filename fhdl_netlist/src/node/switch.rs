use std::{
    fmt::{self, Debug},
    iter::Empty,
};

use fhdl_data_structures::{
    cursor::Cursor,
    graph::{NodeId, Port},
};
use itertools::{Chunk, Chunks, IntoChunks, Itertools};
use smallvec::{smallvec, SmallVec};

use super::{IsNode, MakeNode, NodeKind, NodeOutput};
use crate::{
    const_val::ConstVal, netlist::Module, node_ty::NodeTy, symbol::Symbol,
    with_id::WithId,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Case<T> {
    Val(T),
    Default(u128),
}

impl<T: Debug> Debug for Case<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Val(val) => Debug::fmt(val, f),
            Self::Default(_) => f.write_str("_"),
        }
    }
}

impl<T> Case<T> {
    #[inline]
    pub fn is_default(&self) -> bool {
        matches!(self, Case::Default(_))
    }

    #[inline]
    pub fn default(width: u128) -> Self {
        Self::Default(width)
    }

    pub fn val(&self) -> &T {
        match self {
            Self::Val(val) => val,
            Self::Default(_) => panic!("case value expected"),
        }
    }

    pub fn val_mut(&mut self) -> &mut T {
        match self {
            Self::Val(val) => val,
            Self::Default(_) => panic!("case value expected"),
        }
    }
}

pub type ConstCase = Case<ConstVal>;

impl ConstCase {
    #[inline]
    pub fn width(&self) -> u128 {
        match self {
            Self::Val(val) => val.width(),
            Self::Default(width) => *width,
        }
    }

    pub fn is_match(&self, sel: ConstVal) -> bool {
        match self {
            Self::Val(val) => *val == sel,
            Self::Default(width) => *width == sel.width(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TupleCase(pub SmallVec<[ConstCase; 1]>);

impl Debug for TupleCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[ ")?;
        let mut first = true;
        for case in &self.0 {
            if !first {
                f.write_str(", ")?;
            } else {
                first = false;
            }

            Debug::fmt(case, f)?;
        }
        f.write_str(" ]")?;

        Ok(())
    }
}

impl TupleCase {
    pub fn width(&self) -> u128 {
        self.0.iter().map(|case| case.width()).sum()
    }

    pub fn is_match(&self, sel: ConstVal) -> bool {
        let mut start = 0;
        self.0.iter().rev().all(|val| {
            let width = val.width();
            let is_match = val.is_match(sel.slice(start, width));
            start += width;
            is_match
        })
    }

    pub fn include(&self, other: &Self) -> bool {
        self.0.len() == other.0.len()
            && self.0.iter().zip(other.0.iter()).all(|pair| match pair {
                (Case::Val(this), Case::Val(other)) => this == other,
                (Case::Default(_), _) => true,
                (Case::Val(_), Case::Default(_)) => false,
            })
    }
}

impl Case<TupleCase> {
    pub fn is_match(&self, sel: ConstVal) -> bool {
        match self {
            Self::Val(val) => val.is_match(sel),
            Self::Default(width) => *width == sel.width(),
        }
    }
}

impl From<ConstVal> for TupleCase {
    fn from(val: ConstVal) -> Self {
        TupleCase(smallvec![Case::Val(val)])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Switch {
    pub cases: SmallVec<[Case<TupleCase>; 2]>,
    pub inputs: u32,
    pub outputs: SmallVec<[NodeOutput; 1]>,
    pub default: Option<u32>,
    pub is_single: bool,
}

impl Switch {
    #[inline]
    pub fn has_default(&self) -> bool {
        self.default.is_some()
    }
}

#[derive(Debug)]
pub struct SwitchArgs<V, O, D = Empty<Port>> {
    pub outputs: O,
    pub sel: Port,
    pub variants: V,
    pub default: Option<D>,
}

impl<C, V, I, O, D> MakeNode<SwitchArgs<V, O, D>> for Switch
where
    C: Into<TupleCase>,
    I: IntoIterator<Item = Port>, // inputs for the each variant
    D: IntoIterator<Item = Port>, // inputs for the default variant
    V: IntoIterator<Item = (C, I)>, // variants
    O: IntoIterator<Item = (NodeTy, Option<Symbol>)>, // outputs
{
    fn make(module: &mut Module, args: SwitchArgs<V, O, D>) -> NodeId {
        let sel_width = module[args.sel].width();
        assert!(sel_width != 0);

        let outputs = args
            .outputs
            .into_iter()
            .map(|(ty, sym)| NodeOutput::reg(ty, sym))
            .collect::<SmallVec<_>>();
        let outputs_len = outputs.len();

        let node_id = module.add_node(Switch {
            cases: SmallVec::new(),
            inputs: 0,
            outputs,
            default: None,
            is_single: true,
        });

        module.add_edge(args.sel, Port::new(node_id, 0));

        let mut inputs_len = 1;
        let mut tuple_len = None;

        let variants = args.variants.into_iter();
        let mut cases = SmallVec::with_capacity(variants.size_hint().0);
        for (case, inputs) in variants {
            let case = case.into();
            assert_eq!(*tuple_len.get_or_insert(case.0.len()), case.0.len());
            assert_eq!(case.width(), sel_width);
            cases.push(Case::Val(case));

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
            cases.push(Case::Default(sel_width));

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

        if let NodeKind::Switch(mux) = module[node_id].kind_mut() {
            mux.cases = cases;
            mux.inputs = inputs_len;
            mux.default = default;
            mux.is_single = tuple_len.map(|len| len == 1).unwrap_or_default();
        }

        node_id
    }
}

impl IsNode for Switch {
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
pub type CaseAlias<'n> = impl Iterator<Item = &'n Case<TupleCase>> + Clone + 'n;

pub struct SwitchInputs<'n> {
    pub sel: Port,
    pub cases: Cases<CaseAlias<'n>, PortAlias<'n>>,
    pub is_single: bool,
}

impl<'n> WithId<NodeId, &'n Switch> {
    pub fn inputs(self, module: &'n Module) -> SwitchInputs<'n> {
        let cases = self.cases.iter();
        let mut inputs = module.incoming(self.id).into_iter_(module);
        let sel = inputs.next().unwrap();
        let chunks = inputs.chunks(self.outputs.len());

        SwitchInputs {
            sel,
            cases: Cases { cases, chunks },
            is_single: self.is_single,
        }
    }
}
