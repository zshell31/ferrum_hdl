use crate::{net_list::NetList, symbol::Symbol};

#[derive(Debug)]
pub struct Module {
    pub id: ModuleId,
    pub name: Symbol,
    pub net_list: NetList,
}

impl Module {
    pub fn new(id: ModuleId, name: Symbol) -> Self {
        Self {
            id,
            name,
            net_list: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(usize);

#[derive(Debug, Default)]
pub struct ModuleList(Vec<Module>);

impl ModuleList {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_module(&mut self, name: Symbol) -> ModuleId {
        let module_id = ModuleId(self.0.len());
        self.0.push(Module::new(module_id, name));

        module_id
    }

    pub fn replace(&mut self, module_id: ModuleId, module: Module) {
        self.0[module_id.0] = module;
    }

    pub fn module(&self, module_id: ModuleId) -> &Module {
        &self.0[module_id.0]
    }

    pub fn module_mut(&mut self, module_id: ModuleId) -> &mut Module {
        &mut self.0[module_id.0]
    }

    pub fn iter(&self) -> impl Iterator<Item = &Module> {
        self.0.iter()
    }
}
