use std::collections::VecDeque;

use either::Either;
use fhdl_data_structures::{cursor::Cursor, graph::Port, FxHashSet};

use crate::{
    netlist::{Module, ModuleId, NetList},
    with_id::WithId,
};

pub struct Reachability<'n> {
    netlist: &'n NetList,
    ports: Vec<Port>,
    modules: VecDeque<ModuleId>,
    handled: FxHashSet<ModuleId>,
}

impl<'n> Reachability<'n> {
    pub fn new(netlist: &'n NetList) -> Self {
        Self {
            netlist,
            ports: Default::default(),
            modules: Default::default(),
            handled: Default::default(),
        }
    }

    pub fn run(mut self) {
        if let Some(top) = self.netlist.top {
            self.modules.push_back(top);
        }

        while let Some(module_id) = self.modules.pop_front() {
            if !self.handled.contains(&module_id) {
                let mut module = self.netlist[module_id].borrow_mut();
                self.visit_module(&mut module);

                self.handled.insert(module_id);
            }
        }
    }

    pub(super) fn visit_module(&mut self, module: &mut Module) {
        self.ports.clear();
        self.ports.extend(module.mod_outputs().iter().rev());

        while let Some(port) = self.ports.pop() {
            let node_out = &module[port];
            if !node_out.skip || node_out.ty.width() == 0 {
                continue;
            }

            if let Some(mod_inst) = module[port.node].mod_inst() {
                if !mod_inst.has_ports() {
                    continue;
                }

                self.modules.push_back(mod_inst.mod_id);
            }

            let mut exclude = None;
            if let Some(dff) = module[port.node].dff() {
                let dff = WithId::new(port.node, dff);
                let inputs = dff.inputs(module);
                let init = inputs.init;

                if module.is_const(init)
                    && module
                        .outgoing(init)
                        .into_iter_(module)
                        .all(|node| node == port.node)
                {
                    module[init].skip = true;
                    module[init.node].skip = true;
                    exclude = Some(init);
                }
            }

            module[port].skip = false;
            module[port.node].skip = false;
            module.skip = false;

            let incoming = module.incoming(port.node).into_iter_(module);
            let incoming = if let Some(exclude) = exclude {
                Either::Left(incoming.filter(move |port| *port != exclude))
            } else {
                Either::Right(incoming)
            };
            self.ports.extend(incoming);
        }
    }
}
