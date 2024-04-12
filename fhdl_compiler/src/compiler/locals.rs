use fhdl_data_structures::{idx_ty, tree::Tree, FxHashMap};
use rustc_middle::mir::Local;

use super::item::Item;

idx_ty!(LocalScopeId);

#[derive(Debug, Default, Clone)]
pub struct LocalScope<'tcx>(FxHashMap<Local, Item<'tcx>>);

impl<'tcx> LocalScope<'tcx> {
    pub fn place(&mut self, local: Local, item: Item<'tcx>) -> Local {
        self.0.insert(local, item);
        local
    }

    pub fn get_opt(&self, local: Local) -> Option<Item<'tcx>> {
        self.0.get(&local).cloned()
    }

    pub fn get(&self, local: Local) -> Item<'tcx> {
        match self.0.get(&local) {
            Some(item) => item.clone(),
            None => panic!("cannot find item for local {local:?}"),
        }
    }
}

pub struct Locals<'tcx> {
    tree: Tree<LocalScopeId, LocalScope<'tcx>>,
}

impl<'tcx> Default for Locals<'tcx> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<'tcx> Locals<'tcx> {
    pub fn new() -> Self {
        Self {
            tree: Tree::new_with_root(LocalScope::default()),
        }
    }

    pub fn place(&mut self, local: Local, item: Item<'tcx>) -> Local {
        self.tree.root_mut().unwrap().place(local, item)
    }

    pub fn get_opt(&self, local: Local) -> Option<Item<'tcx>> {
        self.tree.root().unwrap().get_opt(local)
    }

    pub fn get(&self, local: Local) -> Item<'tcx> {
        self.tree.root().unwrap().get(local)
    }

    pub fn add_branch(&mut self, parent_id: LocalScopeId) -> LocalScopeId {
        self.tree.add(parent_id, Default::default())
    }

    pub fn prune_all_branches(&mut self) {
        self.tree.prune_all_except_root()
    }
}
