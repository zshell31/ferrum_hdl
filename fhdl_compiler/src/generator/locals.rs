use rustc_data_structures::fx::FxHashMap;
use rustc_middle::mir::Local as MirLocal;

use super::item::Item;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Local(MirLocal, u32);

impl Local {
    #[allow(dead_code)]
    pub fn mir_local(&self) -> MirLocal {
        self.0
    }

    pub fn inc(&self) -> Local {
        Self(self.0, self.1 + 1)
    }
}

impl From<MirLocal> for Local {
    fn from(local: MirLocal) -> Self {
        Local(local, 0)
    }
}

#[derive(Debug, Default, Clone)]
pub struct Locals<'tcx>(FxHashMap<Local, Item<'tcx>>);

impl<'tcx> Locals<'tcx> {
    pub fn place(&mut self, local: impl Into<Local>, item: Item<'tcx>) -> Local {
        let local = local.into();
        self.0.insert(local, item);
        local
    }

    pub fn get(&self, local: impl Into<Local>) -> &Item<'tcx> {
        let local = local.into();
        match self.0.get(&local) {
            Some(item) => item,
            None => panic!("cannot find item for local {local:?}"),
        }
    }

    pub fn get_mut(&mut self, local: impl Into<Local>) -> &mut Item<'tcx> {
        let local = local.into();
        match self.0.get_mut(&local) {
            Some(item) => item,
            None => panic!("cannot find item for local {local:?}"),
        }
    }
}
