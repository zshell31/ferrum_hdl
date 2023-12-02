use fhdl_netlist::{group::ItemId, net_list::ModuleId, symbol::Symbol};
use rustc_data_structures::fx::FxHashMap;
use rustc_span::symbol::Ident;

#[derive(Debug, Clone, Copy)]
pub enum SymIdent {
    Dff,
    DffEn,
    Msb,
    Out,
}

impl SymIdent {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Dff => "dff",
            Self::DffEn => "dff_en",
            Self::Msb => "msb",
            Self::Out => "__out",
        }
    }
}

impl From<SymIdent> for Symbol {
    #[inline(always)]
    fn from(ident: SymIdent) -> Self {
        Symbol::new(ident.as_str())
    }
}

impl From<SymIdent> for Option<Symbol> {
    #[inline(always)]
    fn from(ident: SymIdent) -> Self {
        Some(ident.into())
    }
}

#[derive(Debug, Default)]
pub struct LocalScope(FxHashMap<Ident, ItemId>);

#[derive(Debug, Default)]
pub struct ModuleScopes {
    scopes: Vec<LocalScope>,
    self_arg: Option<ItemId>,
}

impl ModuleScopes {
    pub fn push_scope(&mut self) {
        self.scopes.push(LocalScope::default());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn add_local_ident(&mut self, ident: Ident, item_id: ItemId) {
        if ident.as_str() == "self" {
            self.self_arg = Some(item_id);
        }

        if let Some(scope) = self.scopes.last_mut() {
            scope.0.insert(ident, item_id);
        }
    }

    pub fn item_id(&self, ident: Ident) -> Option<ItemId> {
        if ident.as_str() == "self" {
            return self.self_arg;
        }

        for scope in self.scopes.iter().rev() {
            if let Some(item_id) = scope.0.get(&ident) {
                return Some(*item_id);
            }
        }

        None
    }
}

#[derive(Default)]
pub struct Scopes {
    modules: FxHashMap<ModuleId, ModuleScopes>,
}

impl Scopes {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn for_module(&mut self, module_id: ModuleId) -> &mut ModuleScopes {
        let module = self.modules.entry(module_id).or_default();
        module
    }
}
