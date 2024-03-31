use rustc_hash::FxHashMap;
use tracing::debug;

use super::{Case, Mux, MuxArgs, MuxInputs, Node};
use crate::{
    const_val::ConstVal,
    netlist::{ListItem, Module, NodeId, Port, WithId},
    utils::IteratorExt,
    visitor::transform::ModCtx,
};

impl Mux {
    pub fn transform_embedded_muxs(
        mux_id: &mut NodeId,
        module: &mut Module,
        mod_ctx: &mut ModCtx,
    ) {
        let mux = module.node(*mux_id).map(|node| node.mux().unwrap());

        if let Some(embedded) = EmbeddedMuxs::collect_embedded_muxs(mux, module) {
            let node = &module[*mux_id];
            let span = node.span().unwrap();

            let cases = &mux.cases;

            #[allow(dead_code)]
            #[derive(Debug)]
            struct Test {
                case: ConstVal,
                span: String,
                nested: Vec<Nested>,
                default: Option<Port>,
            }

            #[allow(dead_code)]
            #[derive(Debug)]
            struct Nested {
                case: ConstVal,
                input: Port,
            }
            debug!(
                "switch '{span}' has embedded switches {:#?}",
                embedded
                    .muxs
                    .iter()
                    .map(|(idx, node_id)| {
                        let node = module.node(*node_id);
                        let span = node.span().unwrap().to_string();
                        let mux = node.map(|node| node.mux().unwrap());

                        let nested = mux
                            .cases(module)
                            .map(|(case, input)| Nested { case, input })
                            .collect();

                        let default = mux.default(module);

                        Test {
                            case: cases[*idx],
                            span,
                            nested,
                            default,
                        }
                    })
                    .collect::<Vec<_>>()
            );
            embedded.transform(mux_id, module, mod_ctx);
        }
    }
}

struct EmbeddedMuxs {
    muxs: FxHashMap<usize, NodeId>,
    total_variants: usize,
    sel: Port,
    sel_width: u128,
    default: Option<Port>,
}

impl EmbeddedMuxs {
    fn collect_embedded_muxs(
        mux: WithId<NodeId, &Mux>,
        module: &Module,
    ) -> Option<EmbeddedMuxs> {
        if !mux.has_default {
            return None;
        }

        let node_span = module[mux.id].span();
        let span = tracing::debug_span!("embedded_muxs", node_span = node_span);
        let _enter = span.enter();

        let mut muxs: FxHashMap<usize, NodeId> = Default::default();

        let mut total_variants = 0;
        for (idx, (_, input)) in mux.inputs(module).cases.enumerate() {
            let case_mux = module.node(input.node).map(Node::mux).as_opt();

            if let Some(case_mux) = case_mux {
                muxs.insert(idx, case_mux.id);
                total_variants += case_mux.cases.len();
                continue;
            }

            total_variants += 1;
        }

        if !muxs.is_empty() {
            let one = muxs
                .values()
                .map(|mux| {
                    let mux = module.node(*mux).map(|node| node.mux().unwrap());
                    let sel = mux.sel(module);
                    let default = mux.default(module);

                    (sel, default)
                })
                .fold_into_one();

            if let Some((sel, default)) = one {
                if let (Some(mux_default), Some(default)) = (mux.default(module), default)
                {
                    if mux_default != default {
                        return None;
                    }
                }
                let sel_width = module[sel].width();

                return Some(EmbeddedMuxs {
                    muxs,
                    sel,
                    sel_width,
                    default,
                    total_variants,
                });
            }
        }

        None
    }

    fn transform(self, mux_id: &mut NodeId, module: &mut Module, mod_ctx: &mut ModCtx) {
        let node = module.node(*mux_id);
        let prev_id = node.prev();

        let mux = node.map(|node| node.mux().unwrap());
        let mux_ty = mux.output[0].ty;
        let mux_sym = mux.output[0].sym;

        let sel1 = mux.sel(module);
        let sel2 = self.sel;

        let default = self.default;
        let variants = self.make_new_variants(module, mux);

        debug!("after transforming: default = {default:?}, variants = {variants:#?}",);

        let new_sel = mod_ctx.get_or_add_combined_sel(module, prev_id, sel1, sel2);

        *mux_id = module.replace::<_, Mux>(*mux_id, MuxArgs {
            ty: mux_ty,
            sym: mux_sym,
            sel: new_sel,
            variants,
            default,
        });
    }

    fn make_new_variants(
        self,
        module: &Module,
        mux: WithId<NodeId, &Mux>,
    ) -> Vec<(ConstVal, Port)> {
        let mut variants = Vec::with_capacity(self.total_variants);

        let MuxInputs { sel, cases } = mux.inputs(module);
        let sel_width = module[sel].width();
        for variant in cases.enumerate() {
            self.extend_variants(module, sel_width, &mut variants, variant);
        }

        variants
    }

    fn extend_variants(
        &self,
        module: &Module,
        sel_width: u128,
        variants: &mut Vec<(ConstVal, Port)>,
        (case_idx, (case1, input1)): (usize, (Case, Port)),
    ) {
        fn shift(case1: Case, sel_width: u128, case2: ConstVal) -> ConstVal {
            match case1 {
                Case::Val(mut case1) | Case::DefaultVal(mut case1) => {
                    case1.shift(case2);
                    case1
                }
                Case::Default => {
                    let mut case1 = ConstVal::zero(sel_width);
                    case1.shift(case2);
                    case1
                }
            }
        }

        match self.muxs.get(&case_idx) {
            Some(mux_id) => {
                let cases = module
                    .node(*mux_id)
                    .map(|node| node.mux().unwrap())
                    .cases(module);

                variants.extend(cases.into_iter().map(move |(case2, input2)| {
                    let case = shift(case1, sel_width, case2);

                    (case, input2)
                }));
            }
            None => {
                if let Case::Default = case1 {
                    return;
                }
                let case = shift(case1, sel_width, ConstVal::zero(self.sel_width));
                variants.push((case, input1));
            }
        }
    }
}
