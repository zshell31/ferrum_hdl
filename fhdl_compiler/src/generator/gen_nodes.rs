use fhdl_netlist::node::GenNode;

use super::Generator;

#[allow(clippy::type_complexity)]
pub struct GenNodes<'tcx> {
    nodes: Vec<Box<dyn FnMut(&mut Generator<'tcx>, GenNode)>>,
}

impl<'tcx> GenNodes<'tcx> {
    pub fn new() -> Self {
        Self {
            nodes: Default::default(),
        }
    }

    pub fn add_fn(
        &mut self,
        gen_fn: impl FnMut(&mut Generator<'tcx>, GenNode) + 'static,
    ) -> u32 {
        let idx = self.nodes.len() as u32;
        self.nodes.push(Box::new(gen_fn));

        idx
    }
}
