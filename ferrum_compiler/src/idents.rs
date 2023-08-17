use ferrum_netlist::{net_list::NodeId, symbol::Symbol};
use rustc_data_structures::fx::FxHashMap;
use rustc_span::symbol::Ident;

#[derive(Debug, Default)]
pub struct LocalScope(FxHashMap<Ident, NodeId>);

#[derive(Debug, Default)]
pub struct Idents {
    scopes: Vec<LocalScope>,
    temp: usize,
    out: usize,
}

impl Idents {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(LocalScope::default());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn tmp(&mut self) -> Symbol {
        Self::temp_ident("__tmp", &mut self.temp)
    }

    pub fn out(&mut self) -> Symbol {
        Self::temp_ident("__out", &mut self.out)
    }

    fn temp_ident(prefix: &str, idx: &mut usize) -> Symbol {
        let sym = if (*idx) == 0 {
            Symbol::new(prefix)
        } else {
            Symbol::new(&format!("{}_{}", prefix, *idx))
        };

        *idx += 1;

        sym
    }

    pub fn add_local_ident(&mut self, ident: Ident, node_idx: NodeId) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.0.insert(ident, node_idx);
        }
    }

    pub fn node_index(&self, ident: Ident) -> Option<NodeId> {
        for scope in self.scopes.iter().rev() {
            if let Some(node_idx) = scope.0.get(&ident) {
                return Some(*node_idx);
            }
        }

        None
    }
}
