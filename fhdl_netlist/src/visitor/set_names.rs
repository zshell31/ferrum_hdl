use fhdl_data_structures::{cursor::Cursor, FxHashMap};

use crate::{
    netlist::{Module, ModuleId, NetList},
    node::{IsNode, Node},
    symbol::Symbol,
    with_id::WithId,
};

pub struct SetNames<'n> {
    netlist: &'n NetList,
    idents: FxHashMap<(ModuleId, Symbol), usize>,
    module_idents: FxHashMap<Symbol, usize>,
}

impl<'n> SetNames<'n> {
    pub fn new(netlist: &'n NetList) -> Self {
        Self {
            netlist,
            idents: Default::default(),
            module_idents: Default::default(),
        }
    }

    pub fn run(mut self) {
        for module in self.netlist.modules() {
            let mut module = module.borrow_mut();
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
        while let Some(node_id) = nodes.next_(*module) {
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

        if let Some(memory) = node.memory_mut() {
            let sym = memory.name.unwrap_or_else(|| Symbol::intern("__mem"));
            let name = self.handle_sym(mod_id, sym);
            memory.name = Some(name);

            let gen_i = Symbol::intern_args(format_args!("{}_i", name));
            memory.gen_i = Some(self.handle_sym(mod_id, gen_i));
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
