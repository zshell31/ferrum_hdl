mod codegen;
mod dump;
mod reachability;
mod set_names;
mod transform;

use codegen::Verilog;
use reachability::Reachability;
use set_names::SetNames;
use transform::Transform;

use self::dump::Dump;
use crate::netlist::{Module, ModuleId, NetList, WithId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamKind {
    Input,
    Output,
}

impl NetList {
    pub fn transform(&mut self) {
        Transform::new().run(self);
    }

    pub fn reachability(&mut self) {
        Reachability::new().run(self);
    }

    pub fn set_names(&mut self) {
        SetNames::new().run(self);
    }

    pub fn synth_verilog(&self) -> String {
        Verilog::new(self).synth()
    }

    pub fn dump(&self, skip: bool) {
        Dump::new(skip).run(self)
    }

    pub fn dump_by_mod_id(&self, mod_id: ModuleId, skip: bool) {
        let module = self.module(mod_id).map(|module| module.borrow());
        Dump::new(skip).visit_module(self, module.as_deref());
    }

    pub fn dump_mod(&self, module: WithId<ModuleId, &Module>, skip: bool) {
        Dump::new(skip).visit_module(self, module);
    }

    pub fn run_visitors(&mut self) {
        self.transform();
        self.reachability();
        self.set_names();
    }
}
