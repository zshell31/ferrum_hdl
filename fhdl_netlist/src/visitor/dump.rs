use crate::{
    net_list::{ModuleId, NetList, NodeId},
    visitor::Visitor,
};

pub struct Dump<'n> {
    netlist: &'n NetList,
    skip: bool,
}

impl<'n> Dump<'n> {
    pub fn new(net_list: &'n NetList, skip: bool) -> Self {
        Self {
            netlist: net_list,
            skip,
        }
    }

    pub fn run(&mut self) {
        self.visit_modules();
    }
}

impl<'n> Visitor for Dump<'n> {
    fn visit_modules(&mut self) {
        for module_id in self.netlist.modules() {
            let module = &self.netlist[module_id];
            if self.skip && module.skip {
                continue;
            }
            self.visit_module(module_id);
        }

        println!("\n");
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        let module = &self.netlist[module_id];
        module.dump(module_id);

        let tab: &'static str = "        ";
        let mut cursor = self.netlist.mod_cursor(module_id);
        while let Some(node_id) = self.netlist.next(&mut cursor) {
            let node = &self.netlist[node_id];

            if self.skip && node.skip {
                continue;
            }

            let prefix = format!("{:>4}    ", node_id.idx());

            node.dump(self.netlist, &prefix, tab);

            println!("\n{}links:", tab);
            for out in node.node_out_ids() {
                if self.netlist.links(out).next().is_some() {
                    println!(
                        "{}{} -> {}",
                        tab,
                        out.idx(),
                        self.netlist
                            .links(out)
                            .map(|node| node.node_id().idx().to_string())
                            .intersperse(", ".to_string())
                            .collect::<String>()
                    );
                }
            }
            println!();
        }
    }

    fn visit_node(&mut self, _node_id: NodeId) {}
}
