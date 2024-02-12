use once_cell::sync::Lazy;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::{
    net_list::{FxIndexSet, ModuleId, NetList, NodeId},
    node::{NodeKindWithId, NodeKindWithIdMut},
    symbol::Symbol,
    visitor::Visitor,
};

static DEFAULT_SYMBOLS: Lazy<FxHashMap<&'static str, &'static str>> = Lazy::new(|| {
    [
        ("input", "_input"),
        ("output", "_output"),
        ("reg", "_reg"),
        ("self", "_self"),
    ]
    .into_iter()
    .collect()
});

pub struct SetNames<'n> {
    net_list: &'n mut NetList,
    idents: FxHashMap<(ModuleId, Symbol), usize>,
    module_idents: FxHashMap<Symbol, usize>,
    node_ids: FxIndexSet<NodeId>,
}

impl<'n> SetNames<'n> {
    pub fn new(net_list: &'n mut NetList) -> Self {
        Self {
            net_list,
            idents: Default::default(),
            module_idents: Default::default(),
            node_ids: Default::default(),
        }
    }

    pub fn run(&mut self) {
        self.visit_modules();
    }

    fn ident(sym: Symbol, count: Option<usize>) -> (Symbol, usize) {
        let sym = DEFAULT_SYMBOLS
            .get(sym.as_str())
            .map(Symbol::new)
            .unwrap_or(sym);

        match count {
            Some(mut count) => {
                count += 1;
                (sym, count)
            }
            None => (sym, 0),
        }
    }

    fn make_sym(sym: Symbol, count: usize) -> Symbol {
        if count == 0 {
            sym
        } else {
            Symbol::new_from_args(format_args!("{}_{}", sym, count))
        }
    }

    fn set_module_name(&mut self, module_id: ModuleId) {
        let module = &mut self.net_list[module_id];
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

        if let NodeKindWithId::ModInst(mod_inst) = self.net_list[node_id].kind() {
            if mod_inst.name().is_none() {
                let sym = Symbol::new_from_args(format_args!(
                    "__{}",
                    self.net_list[mod_inst.module_id()].name
                ));

                let count = self.idents.get(&(module_id, sym)).copied();

                let (new_sym, count) = Self::ident(sym, count);
                self.idents.insert((module_id, sym), count);
                new_name = Some(Self::make_sym(new_sym, count))
            }

            for (output, mod_output) in self.net_list[node_id]
                .outputs()
                .zip(self.net_list.mod_outputs(mod_inst.module_id()))
            {
                if output.sym.is_none() {
                    mod_outputs.push((
                        output.node_out_id().idx(),
                        self.net_list[mod_output].sym,
                    ));
                }
            }
        }

        if let NodeKindWithIdMut::ModInst(mut mod_inst) =
            self.net_list[node_id].kind_mut()
        {
            let name = mod_inst.name().or(new_name);
            mod_inst.set_name(name);

            for (ind, sym) in mod_outputs {
                mod_inst.outputs_mut()[ind].sym = sym;
            }
        }

        let node = &mut self.net_list[node_id];
        for mut out in node.outputs_mut() {
            let sym = out.sym.unwrap_or_else(|| Symbol::new("__tmp"));
            let count = self.idents.get(&(module_id, sym)).copied();

            let (new_sym, count) = Self::ident(sym, count);
            self.idents.insert((module_id, sym), count);
            out.sym = Some(Self::make_sym(new_sym, count));
        }
    }
}

impl<'n> Visitor for SetNames<'n> {
    fn visit_modules(&mut self) {
        for module_id in self.net_list.modules() {
            self.visit_module(module_id);
        }
    }

    fn visit_module(&mut self, module_id: ModuleId) {
        self.set_module_name(module_id);

        self.node_ids.clear();

        self.node_ids.extend(
            self.net_list
                .mod_outputs(module_id)
                .map(|node_out_id| node_out_id.node_id()),
        );

        let mut idx = 0;
        while let Some(node_id) = self.node_ids.get_index(idx) {
            let node_id = *node_id;

            self.set_node_out_names(node_id);

            self.node_ids.extend(
                self.net_list[node_id]
                    .inputs()
                    .map(|node_out_id| node_out_id.node_id()),
            );

            idx += 1;
        }
    }

    fn visit_node(&mut self, _: NodeId) {}
}
