use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use tracing::debug;

use super::{Case, Merger, MergerArgs, Mux, MuxArgs, MuxInputs, Node};
use crate::{
    const_val::ConstVal,
    netlist::{ListItem, Module, NodeId, Port, Symbol, WithId},
    utils::IteratorExt,
};

impl Mux {
    pub fn transform_embedded_muxs(module: &mut Module, mux_id: NodeId) -> bool {
        if let Some(embedded) = EmbeddedMuxs::collect_embedded_muxs(module, mux_id) {
            let node = &module[mux_id];
            let span = node.span().unwrap();

            let mux = node.mux().unwrap();
            let cases = &mux.cases;

            #[allow(dead_code)]
            #[derive(Debug)]
            struct Test {
                case: ConstVal,
                span: String,
                nested: Vec<Nested>,
            }

            #[allow(dead_code)]
            #[derive(Debug)]
            struct Nested {
                case: ConstVal,
                inputs: Vec<Port>,
            }
            debug!(
                "switch '{span}' has embedded switches {:#?}",
                embedded
                    .muxs
                    .iter()
                    .map(|(idx, (node_id, _))| {
                        let node = module.node(*node_id);
                        let span = node.span().unwrap().to_string();

                        let nested = node
                            .map(|node| node.mux().unwrap())
                            .inputs(module)
                            .cases
                            .into_iter()
                            .filter(|(case, _)| !case.is_default())
                            .map(|(case, inputs)| {
                                let case = case.val();
                                let inputs = inputs.collect();

                                Nested { case, inputs }
                            })
                            .collect();

                        Test {
                            case: cases[*idx],
                            span,
                            nested,
                        }
                    })
                    .collect::<Vec<_>>()
            );
            embedded.transform(module, mux_id);
            true
        } else {
            false
        }
    }
}

struct EmbeddedMuxs {
    muxs: FxHashMap<usize, (NodeId, Port)>,
    total_variants: usize,
    sel: Port,
    sel_width: u128,
}

impl EmbeddedMuxs {
    fn collect_embedded_muxs(module: &Module, mux_id: NodeId) -> Option<EmbeddedMuxs> {
        let mux = module.node(mux_id).map(|node| node.mux().unwrap());

        if !mux.has_default() {
            return None;
        }

        let MuxInputs { cases, .. } = mux.inputs(module);
        let mut muxs: FxHashMap<usize, (NodeId, Port)> = Default::default();

        let mut total_variants = 0;
        for (idx, (case, chunk)) in cases.into_iter().enumerate() {
            if let Case::Val(_) = case {
                let case_mux = chunk
                    .map(|input| module.node(input.node).map(Node::mux).as_opt())
                    .into_one_item();

                if let Some(Some(case_mux)) = case_mux {
                    if mux.has_the_same_default(case_mux, module) {
                        let sel = case_mux.sel(module);
                        muxs.insert(idx, (case_mux.id, sel));
                        total_variants += case_mux.cases.len();
                        continue;
                    }
                }
            }

            total_variants += 1;
        }

        if !muxs.is_empty() {
            let sel = muxs.values().map(|(_, sel)| *sel).into_one_item();

            if let Some(sel) = sel {
                let sel_width = module[sel].width();
                return Some(EmbeddedMuxs {
                    muxs,
                    sel,
                    sel_width,
                    total_variants,
                });
            }
        }

        None
    }

    fn transform(self, module: &mut Module, mux_id: NodeId) {
        let node = module.node(mux_id);
        let prev_id = node.prev();
        let mux = node.map(|node| node.mux().unwrap());

        let outputs = mux
            .outputs()
            .map(|out| (out.ty, out.sym))
            .collect::<SmallVec<[_; 1]>>();

        let sel1 = mux.sel(module);
        let sel1_width = module[sel1].width();
        let sel2 = self.sel;
        let sel2_width = self.sel_width;

        let default = Some(mux.default(module).collect::<SmallVec<[_; 1]>>());
        let variants = self.make_new_variants(module, mux);

        debug!("after transforming: {variants:#?}");

        let new_sel = module.insert_and_get_port::<_, Merger>(prev_id, MergerArgs {
            width: sel1_width + sel2_width,
            inputs: [sel1, sel2],
            rev: false,
            sym: Some(Symbol::intern("sel")),
        });

        module.replace::<_, Mux>(mux_id, MuxArgs {
            outputs,
            sel: new_sel,
            variants,
            default,
        });
    }

    fn make_new_variants(
        self,
        module: &Module,
        mux: WithId<NodeId, &Mux>,
    ) -> Vec<(ConstVal, SmallVec<[Port; 1]>)> {
        let mut variants = Vec::with_capacity(self.total_variants);

        let MuxInputs { cases, .. } = mux.inputs(module);
        for variant in cases.into_iter().enumerate() {
            self.extend_variants(module, &mut variants, variant);
        }

        variants
    }

    fn extend_variants(
        &self,
        module: &Module,
        variants: &mut Vec<(ConstVal, SmallVec<[Port; 1]>)>,
        (case_idx, (case1, chunk1)): (usize, (Case, impl Iterator<Item = Port>)),
    ) {
        if case1.is_default() {
            return;
        }

        let mut case1 = case1.val();
        let chunk1 = chunk1.collect::<SmallVec<[_; 1]>>();

        match self.muxs.get(&case_idx) {
            Some((mux_id, _)) => {
                let MuxInputs { cases, .. } = module
                    .node(*mux_id)
                    .map(|node| node.mux().unwrap())
                    .inputs(module);

                variants.extend(
                    cases
                        .into_iter()
                        .filter(|(case2, _)| !case2.is_default())
                        .map(move |(case2, chunk2)| {
                            let chunk2 = chunk2.collect::<SmallVec<[_; 1]>>();

                            let mut case1 = case1;
                            case1.shift(case2.val());
                            (
                                case1,
                                chunk1
                                    .iter()
                                    .map(|port| {
                                        let idx = port.port as usize;
                                        chunk2[idx]
                                    })
                                    .collect::<SmallVec<[_; 1]>>(),
                            )
                        }),
                );
            }
            None => {
                case1.shift(ConstVal::zero(self.sel_width));
                variants.push((case1, chunk1));
            }
        }
    }
}
