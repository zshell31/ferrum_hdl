mod embedded_muxs;

use std::{fmt::Debug, iter};

use either::Either;
use smallvec::SmallVec;

use super::{IsNode, MakeNode, Node, NodeKind, NodeOutput};
use crate::{
    const_val::ConstVal,
    cursor::Cursor,
    netlist::{Module, NodeId, Port, WithId},
    node_ty::NodeTy,
    symbol::Symbol,
    utils::IteratorExt,
    visitor::transform::ModCtx,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mux {
    pub cases: SmallVec<[ConstVal; 2]>,
    pub output: [NodeOutput; 1],
    pub has_default: bool,
}

impl Mux {
    pub fn case(&self, idx: usize) -> Case {
        let case = self.cases[idx];
        if case.is_zero_sized() {
            Case::Default
        } else {
            Case::Val(case)
        }
    }

    pub fn add_multiple_muxs<O, V, D>(
        module: &mut Module,
        discr: Port,
        output_ty: O,
        variants: impl Iterator<Item = (ConstVal, V)>,
        mut default: Option<D>,
        node_span: Option<String>,
    ) -> Vec<Port>
    where
        O: Iterator<Item = NodeTy>,
        V: Iterator<Item = Port>,
        D: Iterator<Item = Port>,
    {
        let mut variants = variants.collect::<Vec<_>>();
        let mut muxs = Vec::with_capacity(output_ty.size_hint().0);

        for output_ty in output_ty {
            let default = default.as_mut().map(|default| default.next().unwrap());

            let variants = variants
                .iter_mut()
                .map(|(case, iter)| (*case, iter.next().unwrap()));

            let mux = MuxArgs {
                ty: output_ty,
                sym: None,
                sel: discr,
                variants,
                default,
            };

            let mux = module.add_and_get_port::<_, Mux>(mux);
            module.add_span(mux.node, node_span.clone());

            muxs.push(mux);
        }

        muxs
    }
}

#[derive(Debug)]
pub struct MuxArgs<V> {
    pub ty: NodeTy,
    pub sym: Option<Symbol>,
    pub sel: Port,
    pub variants: V,
    pub default: Option<Port>,
}

impl<V> MakeNode<MuxArgs<V>> for Mux
where
    V: IntoIterator<Item = (ConstVal, Port)>,
{
    fn make(module: &mut Module, args: MuxArgs<V>) -> NodeId {
        let sel_width = module[args.sel].width();
        assert!(sel_width != 0);

        let node_id = module.add_node(Mux {
            cases: SmallVec::new(),
            output: [NodeOutput::reg(args.ty, args.sym)],
            has_default: args.default.is_some(),
        });

        module.add_edge(args.sel, Port::new(node_id, 0));

        let variants = args.variants.into_iter();
        let mut cases = SmallVec::with_capacity(variants.size_hint().0);
        for (case, input) in variants {
            assert_eq!(case.width(), sel_width);
            assert_eq!(module[input].ty.width(), args.ty.width());

            if Some(input) == args.default {
                continue;
            }

            module.add_edge(input, Port::new(node_id, cases.len() as u32));
            cases.push(case);
        }

        if let Some(input) = args.default {
            assert_eq!(module[input].ty.width(), args.ty.width());

            module.add_edge(input, Port::new(node_id, cases.len() as u32));
            cases.push(ConstVal::default());
        }

        assert!(!cases.is_empty());

        if let NodeKind::Mux(mux) = module[node_id].kind_mut() {
            mux.cases = cases;
        }

        node_id
    }
}

impl IsNode for Mux {
    #[inline]
    fn in_count(&self) -> usize {
        self.cases.len()
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
    DefaultVal(ConstVal),
    Default,
}

pub type CasesIter<'n> = impl Iterator<Item = (Case, Port)> + 'n;

pub struct MuxInputs<'n> {
    pub sel: Port,
    pub cases: CasesIter<'n>,
}

impl<'n> WithId<NodeId, &'n Mux> {
    pub fn inputs(self, module: &'n Module) -> MuxInputs<'n> {
        let cases = if self.has_default {
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
                    .chain(iter::once(Case::DefaultVal(self.cases[len - 1]))),
            )
        };

        let mut inputs = module.incoming(self.id).into_iter_(module);
        let sel = inputs.next().unwrap();
        let cases = cases.zip(inputs);

        MuxInputs { sel, cases }
    }

    pub fn sel(self, module: &Module) -> Port {
        let mut inputs = module.incoming(self.id);
        inputs.next_(module).unwrap()
    }

    pub fn cases(
        self,
        module: &'n Module,
    ) -> impl Iterator<Item = (ConstVal, Port)> + 'n {
        let cases = if self.has_default {
            Either::Left(self.cases.iter().filter_map(|case| {
                if !case.is_zero_sized() {
                    Some(*case)
                } else {
                    None
                }
            }))
        } else {
            Either::Right(self.cases.iter().copied())
        };

        let inputs = module.incoming(self.id).into_iter_(module).skip(1);
        cases.zip(inputs)
    }

    pub fn default(self, module: &Module) -> Option<Port> {
        if self.has_default {
            module
                .incoming(self.id)
                .into_iter_(module)
                .nth(self.cases.len())
        } else {
            None
        }
    }
}

impl Mux {
    pub fn transform(mut mux_id: NodeId, module: &mut Module, mod_ctx: &mut ModCtx) {
        if Self::transform_if_one_or_const_sel(mux_id, module) {
            return;
        }

        Self::transform_embedded_muxs(&mut mux_id, module, mod_ctx);

        Self::exclude_variants(&mut mux_id, module);
    }

    fn transform_if_one_or_const_sel(mux_id: NodeId, module: &mut Module) -> bool {
        let mux = module.node(mux_id).map(|node| node.mux().unwrap());

        let cases_len = mux.cases.len();
        let input = {
            let MuxInputs { sel, cases } = mux.inputs(module);

            let mut cases_ref = cases.into_iter();
            if cases_len == 1 {
                Some(cases_ref.next().unwrap().1)
            } else {
                module.to_const(sel).and_then(|sel| {
                    for (case, chunk) in cases_ref {
                        match case {
                            Case::Val(case) | Case::DefaultVal(case) => {
                                if case == sel {
                                    return Some(chunk);
                                }
                            }
                            Case::Default => {
                                return Some(chunk);
                            }
                        }
                    }

                    None
                })
            }
        };
        if let Some(input) = input {
            module.reconnect_all_outgoing(mux_id, iter::once(input));
            true
        } else {
            false
        }
    }

    fn exclude_variants(mux_id: &mut NodeId, module: &mut Module) -> bool {
        let mux = module.node(*mux_id).map(|node| node.mux().unwrap());

        if mux.has_default {
            let default = mux
                .default(module)
                .map(|port| module.to_port_or_const(port))
                .unwrap();

            // whether there are cases that have inputs equal to default input
            if !mux
                .cases(module)
                .any(|(_, input)| module.to_port_or_const(input) == default)
            {
                return false;
            }

            // if yes, exclude them from variants
            let variants = mux
                .cases(module)
                .filter_map(|(case, input)| {
                    if module.to_port_or_const(input) == default {
                        None
                    } else {
                        Some((case, input))
                    }
                })
                .collect::<Vec<_>>();

            if variants.is_empty() {
                module.reconnect_or_replace(*mux_id, default);
                return true;
            } else {
                let ty = mux.output[0].ty;
                let sym = mux.output[0].sym;
                let sel = mux.sel(module);
                let default = mux.default(module);

                *mux_id = module.replace::<_, Mux>(*mux_id, MuxArgs {
                    ty,
                    sym,
                    sel,
                    variants,
                    default,
                });
                return false;
            }
        } else {
            let one_input = mux
                .inputs(module)
                .cases
                .map(|(_, input)| module.to_port_or_const(input))
                .fold_into_one();

            if let Some(input) = one_input {
                module.reconnect_or_replace(*mux_id, input);

                return true;
            }
        }

        false
    }
}
