use std::{cell::Cell, rc::Rc};

use fhdl_netlist::{group::ItemId, net_list::ModuleId, symbol::Symbol};
use rustc_data_structures::fx::FxHashMap;
use rustc_middle::mir::Local;
use rustc_span::{symbol::Ident, Span};

use crate::error::{Error, SpanError, SpanErrorKind};

#[derive(Debug, Clone, Copy)]
pub enum SymIdent {
    Closure,
    Promoted,
    Part,
    Mux,
    Dff,
    DffEn,
    Msb,
    Out,
}

impl SymIdent {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Closure => "closure",
            Self::Promoted => "promoted",
            Self::Part => "part",
            Self::Mux => "mux",
            Self::Dff => "dff",
            Self::DffEn => "dff_en",
            Self::Msb => "msb",
            Self::Out => "out",
        }
    }
}

impl From<SymIdent> for Symbol {
    #[inline]
    fn from(ident: SymIdent) -> Self {
        Symbol::new(ident.as_str())
    }
}

impl From<SymIdent> for Option<Symbol> {
    #[inline]
    fn from(ident: SymIdent) -> Self {
        Some(ident.into())
    }
}

#[derive(Debug, Default)]
pub struct LocalScope(FxHashMap<Ident, Rc<Cell<ItemId>>>);

#[derive(Debug, Default)]
pub struct ModuleScopes {
    scopes: Vec<LocalScope>,
    self_arg: Option<Rc<Cell<ItemId>>>,
    local_decls: FxHashMap<Local, ItemId>,
}

impl ModuleScopes {
    pub fn push_scope(&mut self) {
        self.scopes.push(LocalScope::default());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn add_self_ident(&mut self, item_id: ItemId) -> Option<Rc<Cell<ItemId>>> {
        self.add_local_ident(Ident::from_str("self"), item_id)
    }

    pub fn add_local_ident(
        &mut self,
        ident: Ident,
        item_id: ItemId,
    ) -> Option<Rc<Cell<ItemId>>> {
        let item_id = Rc::new(Cell::new(item_id));

        if ident.as_str() == "self" {
            self.self_arg = Some(item_id.clone());
            return Some(item_id);
        }

        if let Some(scope) = self.scopes.last_mut() {
            scope.0.insert(ident, item_id.clone());
            Some(item_id)
        } else {
            None
        }
    }

    pub fn replace_local_ident(&mut self, ident: Ident, item_id: ItemId) {
        if ident.as_str() == "self" {
            if let Some(self_arg) = self.self_arg.as_mut() {
                self_arg.replace(item_id);
            }
        }

        for scope in self.scopes.iter_mut().rev() {
            if let Some(old_item_id) = scope.0.get_mut(&ident) {
                old_item_id.replace(item_id);
                break;
            }
        }
    }

    pub fn item_id_opt(&self, ident: Ident) -> Option<Rc<Cell<ItemId>>> {
        if ident.as_str() == "self" {
            return self.self_arg.clone();
        }

        for scope in self.scopes.iter().rev() {
            if let Some(item_id) = scope.0.get(&ident) {
                return Some(item_id.clone());
            }
        }

        None
    }

    pub fn item_id(&self, ident: Ident) -> Result<Rc<Cell<ItemId>>, Error> {
        self.item_id_opt(ident)
            .ok_or_else(|| SpanError::missing_item_id(ident).into())
    }

    pub fn add_local(&mut self, local: Local, item_id: ItemId) {
        self.local_decls.insert(local, item_id);
    }

    pub fn find_local(&mut self, local: Local, span: Span) -> Result<ItemId, Error> {
        self.local_decls.get(&local).copied().ok_or_else(|| {
            SpanError::new(SpanErrorKind::MissingLocal(local), span).into()
        })
    }
}

#[derive(Debug, Default)]
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
