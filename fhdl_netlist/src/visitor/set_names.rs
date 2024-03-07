use rustc_hash::FxHashMap;

use crate::{
    netlist::{Cursor, Module, ModuleId, NetList, WithId},
    node::{IsNode, Node},
    symbol::Symbol,
};

pub struct SetNames {
    idents: FxHashMap<(ModuleId, Symbol), usize>,
    module_idents: FxHashMap<Symbol, usize>,
}

impl SetNames {
    pub fn new() -> Self {
        Self {
            idents: Default::default(),
            module_idents: Default::default(),
        }
    }

    pub fn run(mut self, netlist: &NetList) {
        for module in netlist.modules() {
            let mut module = module.map(|module| module.borrow_mut());
            if module.skip {
                continue;
            }

            self.visit_module(module.as_deref_mut());
        }
    }

    fn visit_module(&mut self, mut module: WithId<ModuleId, &mut Module>) {
        let mod_id = module.id;
        self.set_module_name(*module);

        let mut nodes = module.nodes();
        while let Some(node_id) = nodes.next(*module) {
            let node = &mut module[node_id];

            if !node.skip {
                self.set_node_out_names(mod_id, node);
            }
        }
    }

    fn set_module_name(&mut self, module: &mut Module) {
        let sym = module.name;

        let count = self.module_idents.get(&sym).copied();
        let (new_sym, count) = ident(sym, count);
        self.module_idents.insert(sym, count);

        module.name = make_sym(new_sym, count);
    }

    fn set_node_out_names(&mut self, mod_id: ModuleId, node: &mut Node) {
        if let Some(mod_inst) = node.mod_inst_mut() {
            let sym = mod_inst.name.unwrap_or_else(|| Symbol::intern("__mod"));
            mod_inst.name = Some(self.handle_sym(mod_id, sym));
        }

        for out in node.outputs_mut() {
            let sym = out.sym.unwrap_or_default();
            out.sym = Some(self.handle_sym(mod_id, sym));
        }
    }

    fn handle_sym(&mut self, mod_id: ModuleId, sym: Symbol) -> Symbol {
        let count = self.idents.get(&(mod_id, sym)).copied();
        let (new_sym, count) = ident(sym, count);
        self.idents.insert((mod_id, sym), count);

        make_sym(new_sym, count)
    }
}

fn ident(sym: Symbol, count: Option<usize>) -> (Symbol, usize) {
    match count {
        Some(mut count) => {
            count += 1;
            (sym, count)
        }
        None => (sym, 0),
    }
}

fn make_sym(sym: Symbol, count: usize) -> Symbol {
    if sym.is_empty() {
        Symbol::intern_args(format_args!("_${}", count + 1))
    } else if count == 0 {
        sym
    } else {
        Symbol::intern_args(format_args!("{}_{}", sym, count))
    }
}
