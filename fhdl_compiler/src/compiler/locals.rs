use std::mem;

use either::Either;
use fhdl_data_structures::{cursor::Cursor, idx_ty, tree::Tree};
use rustc_data_structures::fx::{FxHashMap, FxHashSet, FxIndexSet};
use rustc_middle::mir::Local;

use super::{item::Item, utils::Captures};
use crate::error::Error;

idx_ty!(LocalScopeId);

#[derive(Debug, Default, Clone)]
pub struct LocalScope<'tcx> {
    locals: FxHashMap<Local, Item<'tcx>>,
    target: Option<u32>,
    otherwise: Option<LocalScopeId>,
    branch_locals: FxIndexSet<Local>,
}

impl<'tcx> LocalScope<'tcx> {
    fn variant(target: usize) -> Self {
        Self {
            target: Some(target as u32),
            ..Default::default()
        }
    }

    #[inline]
    fn place(&mut self, local: Local, item: Item<'tcx>) -> Local {
        self.locals.insert(local, item);
        local
    }

    #[inline]
    fn get_opt(&self, local: Local) -> Option<Item<'tcx>> {
        self.locals.get(&local).cloned()
    }

    #[inline]
    fn get_opt_mut(&mut self, local: Local) -> Option<&mut Item<'tcx>> {
        self.locals.get_mut(&local)
    }

    #[inline]
    fn has_local(&self, local: Local) -> bool {
        self.locals.contains_key(&local)
    }

    fn assign_branch_locals(&mut self, mux: Item<'tcx>) {
        for (local, item) in self.branch_locals.iter().zip(&*mux.group().items()) {
            self.locals.insert(*local, item.clone());
        }
    }
}

type ScopeTree<'tcx> = Tree<LocalScopeId, LocalScope<'tcx>>;

#[derive(Debug)]
pub struct Locals<'tcx> {
    tree: ScopeTree<'tcx>,
    scope_id: LocalScopeId,
}

impl<'tcx> Default for Locals<'tcx> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

pub type ChildrenIter<'l, 'tcx: 'l> =
    impl Iterator<Item = LocalScopeId> + Captures<'tcx> + 'l;
pub type VariantItemIter<'l, 'tcx: 'l> = impl Iterator<Item = Item<'tcx>> + 'l;

pub struct Variants<'l, 'tcx: 'l> {
    tree: &'l Locals<'tcx>,
    children: ChildrenIter<'l, 'tcx>,
    locals: &'l FxIndexSet<Local>,
}

impl<'l, 'tcx: 'l> Iterator for Variants<'l, 'tcx> {
    type Item = (usize, VariantItemIter<'l, 'tcx>);

    fn next(&mut self) -> Option<Self::Item> {
        let child = &self.tree.tree[self.children.next()?].data;

        Some((
            child.target.unwrap() as usize,
            self.locals.iter().map(|local| {
                child
                    .get_opt(*local)
                    .unwrap_or_else(|| self.tree.get(*local))
            }),
        ))
    }
}

impl<'tcx> Locals<'tcx> {
    pub fn new() -> Self {
        let tree = Tree::new_with_root(LocalScope::default());
        let scope_id = tree.root_id().unwrap();

        Self { tree, scope_id }
    }

    pub fn place(&mut self, local: Local, item: Item<'tcx>) -> Local {
        self.tree[self.scope_id].data.place(local, item)
    }

    pub fn get_opt(&self, local: Local) -> Option<Item<'tcx>> {
        self.get_opt_(self.scope_id, local)
    }

    fn get_opt_(&self, scope_id: LocalScopeId, local: Local) -> Option<Item<'tcx>> {
        let mut parent_id = Some(scope_id);
        while let Some(scope_id) = parent_id {
            match self.tree[scope_id].data.get_opt(local) {
                Some(item) => {
                    return Some(item);
                }
                None => {
                    parent_id = self.tree[scope_id].parent();
                }
            }
        }

        None
    }

    pub fn get(&self, local: Local) -> Item<'tcx> {
        match self.get_opt(local) {
            Some(item) => item,
            None => panic!("cannot find item for local {local:?}"),
        }
    }

    pub fn get_opt_mut<'a>(
        &'a mut self,
        scope_id: LocalScopeId,
        local: Local,
    ) -> Option<&'a mut Item<'tcx>>
    where
        'tcx: 'a,
    {
        let mut parent_id = Some(scope_id);
        while let Some(scope_id) = parent_id {
            if let Some(item) = self.tree[scope_id].data.get_opt_mut(local) {
                let item = unsafe { mem::transmute::<_, &'a mut Item<'tcx>>(item) };
                return Some(item);
            }

            parent_id = self.tree[scope_id].parent();
        }

        None
    }

    pub fn get_mut(&mut self, local: Local) -> &mut Item<'tcx> {
        match self.get_opt_mut(self.scope_id, local) {
            Some(item) => item,
            None => panic!("cannot find item for local {local:?}"),
        }
    }

    pub fn has_local(&self, local: Local) -> bool {
        self.tree[self.scope_id].data.has_local(local)
    }

    pub fn has_branches(&self) -> bool {
        self.tree[self.scope_id].has_children()
    }

    pub fn is_root(&self) -> bool {
        self.scope_id == self.tree.root_id().unwrap()
    }

    pub fn go_to_variant(&mut self, target: usize) {
        self.scope_id = self.tree.add(self.scope_id, LocalScope::variant(target));
    }

    pub fn go_to_otherwise(&mut self) {
        let parent_id = self.scope_id;
        self.scope_id = self.tree.add(self.scope_id, LocalScope::default());
        self.tree[parent_id].data.otherwise = Some(self.scope_id);
    }

    pub fn leave_branch(&mut self) {
        self.scope_id = self.tree[self.scope_id].parent().unwrap();
    }

    pub fn branch_locals(&self) -> &FxIndexSet<Local> {
        &self.tree[self.scope_id].data.branch_locals
    }

    fn branch_locals_<'l>(
        &'l self,
        scope_id: LocalScopeId,
    ) -> impl Iterator<Item = Local> + Captures<'tcx> + 'l
    where
        'tcx: 'l,
    {
        if !self.tree[scope_id].has_children() {
            Either::Left(self.tree[scope_id].data.locals.keys().copied())
        } else {
            Either::Right(self.tree[scope_id].data.branch_locals.iter().copied())
        }
    }

    pub fn collect_branch_locals(&mut self) -> Result<(), Error> {
        if !self.has_branches() {
            return Ok(());
        }

        let scope_id = self.scope_id;
        let mut inner = FxHashMap::<Local, u32>::default();
        let mut outer = FxHashSet::<Local>::default();

        let mut branches = 0;
        for child_id in self.tree[scope_id].children().into_iter_(&self.tree) {
            branches += 1;

            for local in self.branch_locals_(child_id) {
                // is local outer?
                if self.get_opt_(scope_id, local).is_some() {
                    outer.insert(local);
                } else {
                    *inner.entry(local).or_default() += 1;
                }
            }
        }

        tracing::debug!("inner: {inner:?}");
        tracing::debug!("outer: {outer:?}");

        let mut locals: FxIndexSet<Local> = Default::default();

        if !inner.is_empty() {
            locals.extend(
                inner
                    .into_iter()
                    .filter(|(_, count)| *count == branches)
                    .map(|(local, _)| local),
            );
        }
        locals.extend(outer);
        locals.sort_unstable();

        self.tree[scope_id].data.branch_locals = locals;

        Ok(())
    }

    pub fn otherwise(&self) -> Option<impl Iterator<Item = Item<'tcx>> + '_> {
        let scope = &self.tree[self.scope_id].data;
        let otherwise = &self.tree[scope.otherwise?].data;

        Some(scope.branch_locals.iter().map(|local| {
            otherwise
                .get_opt(*local)
                .unwrap_or_else(|| self.get(*local))
        }))
    }

    pub fn variants<'l>(&'l self) -> Variants<'l, 'tcx>
    where
        'tcx: 'l,
    {
        let scope = &self.tree[self.scope_id];

        Variants {
            tree: self,
            children: scope
                .children()
                .into_iter_(&self.tree)
                .filter(|child_id| self.tree[*child_id].data.target.is_some()),
            locals: &scope.data.branch_locals,
        }
    }

    pub fn assign_branch_locals(&mut self, mux: Item<'tcx>) {
        let scope = &mut self.tree[self.scope_id].data;
        scope.assign_branch_locals(mux);
    }

    pub fn prune_all_branches(&mut self) {
        self.tree.prune_all_except_root()
    }
}
