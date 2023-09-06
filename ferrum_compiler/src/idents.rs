use ferrum_netlist::{group_list::ItemId, net_list::ModuleId, symbol::Symbol};
use rustc_data_structures::fx::FxHashMap;
use rustc_span::symbol::Ident;

#[derive(Debug, Default)]
pub struct LocalScope(FxHashMap<Ident, ItemId>);

#[derive(Debug, Default)]
pub struct ModuleIdents {
    scopes: Vec<LocalScope>,
    idents: FxHashMap<String, usize>,
}

impl ModuleIdents {
    pub fn push_scope(&mut self) {
        self.scopes.push(LocalScope::default());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn ident_inner(&mut self, ident: &str) -> Symbol {
        // TODO: check if ident is keyword
        let ident = match ident {
            "input" => "input$",
            "output" => "output$",
            "reg" => "reg$",
            _ => ident,
        };

        match self.idents.get_mut(ident) {
            Some(count) => {
                *count += 1;
                Symbol::new(&format!("{}_{}", ident, count))
            }
            None => {
                self.idents.insert(ident.to_string(), 0);
                Symbol::new(ident)
            }
        }
    }

    pub fn ident(&mut self, ident: Ident) -> Symbol {
        self.ident_inner(ident.as_str())
    }

    pub fn tmp(&mut self) -> Symbol {
        self.ident_inner("__tmp")
    }

    pub fn out(&mut self) -> Symbol {
        self.ident_inner("__out")
    }

    pub fn inst(&mut self, module: Symbol) -> Symbol {
        self.ident_inner(&format!("__{}", module))
    }

    pub fn add_local_ident(&mut self, ident: Ident, item_id: ItemId) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.0.insert(ident, item_id);
        }
    }

    pub fn item_id(&self, ident: Ident) -> Option<ItemId> {
        for scope in self.scopes.iter().rev() {
            if let Some(item_id) = scope.0.get(&ident) {
                return Some(*item_id);
            }
        }

        None
    }
}

#[derive(Default)]
pub struct Idents {
    modules: FxHashMap<ModuleId, ModuleIdents>,
}

impl Idents {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn module(&self, ident: Ident) -> Symbol {
        Symbol::new(ident.as_str())
    }

    pub fn for_module(&mut self, module_id: ModuleId) -> &mut ModuleIdents {
        let module = self.modules.entry(module_id).or_default();
        module
    }
}
