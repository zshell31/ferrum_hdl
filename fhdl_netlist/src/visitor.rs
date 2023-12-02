mod dump;
mod inject_nodes;
mod reachability;
mod set_names;
mod transform;

use dump::Dump;
use inject_nodes::InjectNodes;
use reachability::Reachability;
use set_names::SetNames;
use transform::Transform;

use crate::net_list::{ModuleId, NetList, NodeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamKind {
    Input,
    Output,
}

pub trait Visitor {
    fn visit_modules(&mut self);

    fn visit_module(&mut self, module_id: ModuleId);

    fn visit_node(&mut self, node_id: NodeId);
}

impl NetList {
    pub fn transform(&mut self) {
        Transform::new(self).run();
    }

    pub fn reachability(&mut self) {
        Reachability::new(self).run();
    }

    pub fn set_names(&mut self) {
        SetNames::new(self).run();
    }

    pub fn inject_nodes(&mut self) {
        InjectNodes::new(self).run();
    }

    pub fn dump(&self, skip: bool) {
        Dump::new(self, skip).run()
    }

    pub fn dump_mod(&self, mod_id: ModuleId, skip: bool) {
        Dump::new(self, skip).visit_module(mod_id);
    }
}
