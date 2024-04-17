use fhdl_data_structures::{cursor::Cursor, index::IndexType};

use crate::{
    netlist::{Module, ModuleId, NetList},
    with_id::WithId,
};

pub struct Dump<'n> {
    netlist: &'n NetList,
    skip: bool,
}

impl<'n> Dump<'n> {
    pub fn new(netlist: &'n NetList, skip: bool) -> Self {
        Self { netlist, skip }
    }

    pub fn run(&mut self) {
        for module in self.netlist.modules().rev() {
            let module = module.borrow();
            if self.skip && module.skip {
                continue;
            }
            self.visit_module(module.as_deref());
        }

        println!("\n");
    }

    pub(super) fn visit_module(&mut self, module: WithId<ModuleId, &Module>) {
        module.dump();

        let tab: &'static str = "        ";

        let mut nodes = module.nodes();
        while let Some(node_id) = nodes.next_(*module) {
            let node = module.node(node_id);

            if self.skip && node.skip {
                continue;
            }

            let prefix = format!("{:>4}    ", node_id.as_u32());

            node.dump(self.netlist, *module, &prefix, tab);

            println!("\n{}links:", tab);
            for port in node.out_ports() {
                let links = module
                    .outgoing(port)
                    .into_iter_(*module)
                    .map(|node_id| node_id.as_u32().to_string())
                    .intersperse(", ".to_string())
                    .collect::<String>();
                if !links.is_empty() {
                    println!("{}{} -> {}", tab, port, links);
                }
            }
            println!();
        }
    }
}
