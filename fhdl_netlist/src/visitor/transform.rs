use smallvec::SmallVec;

use crate::{
    cfg::InlineMod,
    const_val::ConstVal,
    netlist::{Cursor, Module, ModuleId, NetList, NodeId, Port},
    node::{
        BinOp, BinOpInputs, Case, Const, ConstArgs, DFFArgs, DFFInputs, MultiConst,
        MuxInputs, NodeKind, TyOrData, DFF,
    },
};

const NODES_LIMIT_TO_INLINE: usize = 10;

pub struct Transform {}

impl Transform {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(mut self, netlist: &mut NetList) {
        if let Some(top) = netlist.top {
            self.visit_module(netlist, top);
        }
    }

    fn visit_module(&mut self, netlist: &NetList, mod_id: ModuleId) {
        let mut module = netlist.module(mod_id).map(|module| module.borrow_mut());
        let mut nodes = module.nodes();

        while let Some(node_id) = nodes.next(&module) {
            if let Some(mod_inst) = module[node_id].mod_inst() {
                let mod_id = mod_inst.mod_id;

                // transform nodes of module before inlining module
                self.visit_module(netlist, mod_id);
            }

            let should_be_inlined = self.transform(netlist, &mut module, node_id);

            if should_be_inlined {
                let node_id = netlist.inline_mod(module.as_deref_mut(), node_id);

                if let Some(node_id) = node_id {
                    nodes.set_next(node_id);
                }
            }
        }
    }

    fn transform(
        &mut self,
        netlist: &NetList,
        module: &mut Module,
        node_id: NodeId,
    ) -> bool {
        let node = module.node(node_id);

        let mut inline = false;
        match &*node.kind {
            NodeKind::Pass(pass) => {
                match module.to_const(node.with(pass).input(module)) {
                    Some(const_val) => {
                        let output = pass.output[0];

                        module.replace::<_, Const>(node_id, ConstArgs {
                            ty: output.ty,
                            value: const_val.val(),
                            sym: output.sym,
                        });
                    }
                    None => {
                        if !module.is_output(Port::new(node_id, 0)) {
                            module.reconnect(node_id);
                        }
                    }
                }
            }
            NodeKind::ModInst(mod_inst) => {
                let orig_module = netlist[mod_inst.mod_id].borrow();

                if orig_module.has_const_outputs() {
                    let const_args = orig_module.outputs().iter().map(|port| {
                        let const_val = orig_module.to_const(*port).unwrap();
                        let port = orig_module[*port];

                        ConstArgs {
                            ty: port.ty,
                            value: const_val.val(),
                            sym: port.sym,
                        }
                    });

                    module.replace::<_, MultiConst>(node_id, const_args);
                } else {
                    match netlist.cfg().inline_mod {
                        InlineMod::All => {
                            inline = true;
                        }
                        InlineMod::Auto => {
                            inline = orig_module.inline
                                || module.in_count() == 0
                                || module.out_count() == 0
                                || module.node_count() <= NODES_LIMIT_TO_INLINE
                                || module.node_has_const_inputs(node_id)
                        }
                        InlineMod::None => {
                            inline = false;
                        }
                    };
                }
            }
            NodeKind::BitNot(bit_not) => {
                if let Some(const_val) = module.to_const(node.with(bit_not).input(module))
                {
                    let const_val = !const_val;
                    let output = bit_not.output[0];
                    module.replace::<_, Const>(node_id, ConstArgs {
                        ty: output.ty,
                        value: const_val.val(),
                        sym: output.sym,
                    });
                }
            }

            NodeKind::BinOp(bin_op) => {
                let BinOpInputs { lhs, rhs } = node.with(bin_op).inputs(module);

                if let (Some(left), Some(right)) =
                    (module.to_const(lhs), module.to_const(rhs))
                {
                    let const_val = match bin_op.bin_op {
                        BinOp::Add => left + right,
                        BinOp::Sub => left - right,
                        BinOp::Mul => left * right,
                        BinOp::Div => left / right,
                        BinOp::BitAnd => left & right,
                        BinOp::Rem => left % right,
                        BinOp::BitOr => left | right,
                        BinOp::BitXor => left ^ right,
                        BinOp::And => left & right,
                        BinOp::Or => left & right,
                        BinOp::Shl => left << right,
                        BinOp::Shr => left >> right,
                        BinOp::Eq => (left == right).into(),
                        BinOp::Ne => (left != right).into(),
                        BinOp::Ge => (left >= right).into(),
                        BinOp::Gt => (left > right).into(),
                        BinOp::Le => (left <= right).into(),
                        BinOp::Lt => (left < right).into(),
                    };

                    let output = bin_op.output[0];

                    module.replace::<_, Const>(node_id, ConstArgs {
                        ty: output.ty,
                        value: const_val.val(),
                        sym: output.sym,
                    });
                }
            }
            NodeKind::Splitter(splitter) => {
                let splitter = node.with(splitter);

                if splitter.pass_all_bits(module) {
                    module.reconnect(node_id);
                } else {
                    let indices = splitter.eval_indices(module);
                    let input = splitter.input(module);
                    let input_val = module.to_const(input);

                    if let Some(input_val) = input_val {
                        let input_val = input_val.val();
                        let const_args = indices
                            .map(|(index, output)| {
                                let value =
                                    ConstVal::new(input_val >> index, output.width())
                                        .val();

                                ConstArgs {
                                    ty: output.ty,
                                    value,
                                    sym: output.sym,
                                }
                            })
                            .collect::<SmallVec<[ConstArgs; 1]>>()
                            .into_iter();

                        module.replace::<_, MultiConst>(node_id, const_args);
                    } else {
                        drop(indices);

                        let input_id = splitter.input(module).node;
                        let input = &module[input_id];

                        if let NodeKind::Merger(merger) = &*input.kind {
                            if splitter.rev != merger.rev // TODO: maybe it's unnecessary
                                    && module.is_reversible(input_id, node_id)
                            {
                                module
                                    .reconnect_from_inputs_to_outputs(input_id, node_id);
                            }
                        }
                    }
                }
            }
            NodeKind::Merger(merger) => {
                // let sym = output.sym;

                // if !*rev {
                //     // for case:
                //     // ```verilog
                //     // wire [3:0] in;
                //     // wire [3:0] out;
                //     // assing out = {in[3], in[2], in[1], in[0]};
                //     // ```
                //     //
                //     // after transform:
                //     // ```verilog
                //     // wire [3:0] in;
                //     // wire [3:0] out;
                //     // assign out = in;
                //     // ```
                //     if let Some(node_out_id) = self.is_merger_eq_input(inputs) {
                //         let out = &self[node_out_id];
                //         return Pass::new(out.ty, node_out_id, sym).into();
                //     }
                // }

                let mut val = Some(ConstVal::new(0, 0));
                node.with(merger).inputs(module).for_each(|input| {
                    match module.to_const(input) {
                        Some(new_val) => {
                            if let Some(val) = val.as_mut() {
                                val.shift(new_val);
                            }
                        }
                        None => {
                            val = None;
                        }
                    }
                });

                if let Some(const_val) = val {
                    let output = merger.output[0];
                    module.replace::<_, Const>(node_id, ConstArgs {
                        ty: output.ty,
                        value: const_val.val(),
                        sym: output.sym,
                    });
                }
            }

            NodeKind::ZeroExtend(zero_extend) => {
                let zero_extend = node.with(zero_extend);
                let output = zero_extend.output[0];
                let input = zero_extend.input(module);

                match module.to_const(input) {
                    Some(const_val) => {
                        module.replace::<_, Const>(node_id, ConstArgs {
                            ty: output.ty,
                            value: const_val.val(),
                            sym: output.sym,
                        });
                    }
                    None => {
                        if module[input].width() == output.width() {
                            module.reconnect(node_id);
                        }
                    }
                }
            }

            NodeKind::Mux(mux) => {
                let mux = node.with(mux);
                let MuxInputs { sel, cases } = mux.inputs(module);

                if let Some(new_port) = module.to_const(sel).and_then(|sel| {
                    for (case, input) in cases {
                        match case {
                            Case::Val(case) => {
                                if case == sel {
                                    return Some(input);
                                }
                            }
                            Case::Default => {
                                return Some(input);
                            }
                        }
                    }

                    None
                }) {
                    let old_port = Port::new(node_id, 0);
                    module.reconnect_all_outgoing(old_port, new_port);
                }
            }

            NodeKind::DFF(dff) => {
                let dff = node.with(dff);
                let DFFInputs {
                    clk,
                    mut rst,
                    mut en,
                    init,
                    data,
                } = dff.inputs(module);

                let mut replace = false;

                let mut true_rst = false;
                if let Some(const_val) = rst.and_then(|rst| module.to_const(rst)) {
                    if dff.rst_pol.bool(const_val.val() == 0) {
                        rst = None;
                        replace = true;
                    } else {
                        true_rst = true;
                    }
                }

                let mut false_en = false;
                if let Some(const_val) = en.and_then(|en| module.to_const(en)) {
                    if const_val.val() > 0 {
                        en = None;
                        replace = true;
                    } else {
                        false_en = true;
                    }
                };

                if replace {
                    module.replace::<_, DFF>(node_id, DFFArgs {
                        rst_kind: dff.rst_kind,
                        rst_pol: dff.rst_pol,
                        clk,
                        rst,
                        en,
                        init,
                        data: TyOrData::Data(data),
                        sym: dff.output[0].sym,
                    });
                } else if true_rst || false_en {
                    let old_port = Port::new(node_id, 0);
                    module.reconnect_all_outgoing(old_port, init);
                }
            }
            _ => {}
        };

        inline
    }
}
