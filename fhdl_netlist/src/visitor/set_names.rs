use once_cell::sync::Lazy;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::{
    net_list::{ModuleId, NetList, NodeId},
    node::{NodeKindWithId, NodeKindWithIdMut},
    symbol::Symbol,
    visitor::Visitor,
};

static DEFAULT_SYMBOLS: Lazy<FxHashSet<&'static str>> =
    Lazy::new(|| ["input", "output", "reg", "self"].into_iter().collect());

pub struct SetNames<'n> {
    netlist: &'n mut NetList,
    idents: FxHashMap<(ModuleId, Symbol), usize>,
    module_idents: FxHashMap<Symbol, usize>,
}

impl<'n> SetNames<'n> {
    pub fn new(net_list: &'n mut NetList) -> Self {
        Self {
            netlist: net_list,
            idents: Default::default(),
            module_idents: Default::default(),
        }
    }

    pub fn run(&mut self) {
        self.visit_modules();
    }

    fn ident(sym: Symbol, count: Option<usize>) -> (Symbol, usize) {
        let sym = if !sym.is_empty() {
            let (suffix, prefix) = match sym.split_once_with_delim('$') {
                Some(splitted) => splitted,
                None => (sym.as_str(), ""),
            };

            if DEFAULT_SYMBOLS.contains(suffix) || !prefix.is_empty() {
                Symbol::new_from_args(format_args!("_{sym}"))
            } else {
                sym
            }
        } else {
            sym
        };

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
            Symbol::new_from_args(format_args!("_${}", count + 1))
        } else if count == 0 {
            sym
        } else {
            Symbol::new_from_args(format_args!("{}_{}", sym, count))
        }
    }

    fn set_module_name(&mut self, module_id: ModuleId) {
        let module = &mut self.netlist[module_id];
        let sym = module.name;
        let count = self.module_idents.get(&sym).copied();

        let (new_sym, count) = Self::ident(sym, count);
        self.module_idents.insert(sym, count);
        module.name = Self::make_sym(new_sym, count);
    }

    fn set_node_out_names(&mut self, node_id: NodeId) {
        let module_id = node_id.module_id();

        let mut new_name = None;
        let mut mod_outputs = SmallVec::<[_; 8]>::new();

        if let NodeKindWithId::ModInst(mod_inst) = self.netlist[node_id].kind() {
            if mod_inst.name().is_none() {
                let sym = Symbol::new("__mod");
                // let sym = Symbol::new_from_args(format_args!(
                //     "__{}",
                //     self.netlist[mod_inst.module_id()].name
                // ));

                let count = self.idents.get(&(module_id, sym)).copied();

                let (new_sym, count) = Self::ident(sym, count);
                self.idents.insert((module_id, sym), count);
                new_name = Some(Self::make_sym(new_sym, count))
            }

            for (output, mod_output) in self.netlist[node_id]
                .outputs()
                .zip(self.netlist.mod_outputs(mod_inst.module_id()))
            {
                if output.sym.is_none() {
                    mod_outputs
                        .push((output.node_out_id().idx(), self.netlist[mod_output].sym));
                }
            }
        }

        if let NodeKindWithIdMut::ModInst(mut mod_inst) = self.netlist[node_id].kind_mut()
        {
            let name = mod_inst.name().or(new_name);
            mod_inst.set_name(name);

            for (ind, sym) in mod_outputs {
                mod_inst.outputs_mut()[ind].sym = sym;
            }
        }

        let node = &mut self.netlist[node_id];
        for mut out in node.outputs_mut() {
            let sym = out.sym.unwrap_or_default();
            let count = self.idents.get(&(module_id, sym)).copied();

            let (new_sym, count) = Self::ident(sym, count);
            self.idents.insert((module_id, sym), count);
            out.sym = Some(Self::make_sym(new_sym, count));
        }
    }
}

impl<'n> Visitor for SetNames<'n> {
    fn visit_modules(&mut self) {
        for module_id in self.netlist.modules() {
            if self.netlist[module_id].skip {
                continue;
            }

            self.visit_module(module_id);
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        self.set_module_name(module_id);

        let mut cursor = self.netlist.mod_cursor(module_id);
        while let Some(node_id) = self.netlist.next(&mut cursor) {
            let node = &self.netlist[node_id];

            if !node.skip {
                self.set_node_out_names(node_id);
            }
        }
    }

    fn visit_node(&mut self, _: NodeId) {}
}
