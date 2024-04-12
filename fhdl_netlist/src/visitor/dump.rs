use fhdl_data_structures::{cursor::Cursor, index::IndexType};

use crate::{
    netlist::{Module, ModuleId, NetList},
    with_id::WithId,
};

pub struct Dump {
    skip: bool,
}

impl Dump {
    pub fn new(skip: bool) -> Self {
        Self { skip }
    }

    pub fn run(&mut self, netlist: &NetList) {
        for module in netlist.modules().rev() {
            let module = module.borrow();
            if self.skip && module.skip {
                continue;
            }
            self.visit_module(netlist, module.as_deref());
        }

        println!("\n");
    }

    pub(super) fn visit_module(
        &mut self,
        netlist: &NetList,
        module: WithId<ModuleId, &Module>,
    ) {
        module.dump();

        let tab: &'static str = "        ";

        let mut nodes = module.nodes();
        while let Some(node_id) = nodes.next_(*module) {
            let node = module.node(node_id);

            if self.skip && node.skip {
                continue;
            }

            let prefix = format!("{:>4}    ", node_id.as_u32());

            node.dump(netlist, *module, &prefix, tab);

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
