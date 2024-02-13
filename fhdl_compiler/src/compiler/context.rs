use std::collections::{BTreeMap, BTreeSet};

use fhdl_netlist::net_list::ModuleId;
use petgraph::{
    algo::dominators::{simple_fast, Dominators},
    prelude::NodeIndex,
    stable_graph::IndexType,
    Directed, Graph,
};
use rustc_data_structures::{
    fx::{FxHashMap, FxHashSet},
    graph::WithSuccessors,
};
use rustc_hir::def_id::DefId;
use rustc_middle::{
    mir::{
        BasicBlock, BasicBlocks, Body, Const as MirConst, Local, TerminatorKind,
        START_BLOCK,
    },
    ty::{EarlyBinder, GenericArgsRef, TyCtxt},
};
use rustc_type_ir::fold::TypeFoldable;

use crate::compiler::item::Item;

#[derive(Debug, Default, Clone)]
pub struct Locals<'tcx>(FxHashMap<Local, Item<'tcx>>);

impl<'tcx> Locals<'tcx> {
    pub fn place(&mut self, local: Local, item: Item<'tcx>) -> Local {
        self.0.insert(local, item);
        local
    }

    pub fn get(&self, local: Local) -> &Item<'tcx> {
        match self.0.get(&local) {
            Some(item) => item,
            None => panic!("cannot find item for local {local:?}"),
        }
    }

    pub fn get_mut(&mut self, local: Local) -> &mut Item<'tcx> {
        match self.0.get_mut(&local) {
            Some(item) => item,
            None => panic!("cannot find item for local {local:?}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variant<'tcx> {
    pub value: u128,
    pub locals: BTreeMap<Local, Item<'tcx>>,
}

impl<'tcx> Variant<'tcx> {
    fn new(value: u128) -> Self {
        Self {
            value,
            locals: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Switch<'tcx> {
    pub locals: BTreeSet<Local>,
    pub discr: Item<'tcx>,
    targets: usize,
    variants: Vec<Variant<'tcx>>,
    is_otherwise: bool,
    otherwise: Option<BTreeMap<Local, Item<'tcx>>>,
}

impl<'tcx> Switch<'tcx> {
    pub fn new(locals: FxHashSet<Local>, discr: Item<'tcx>, targets: usize) -> Self {
        Self {
            locals: locals.into_iter().collect(),
            discr,
            targets,
            variants: Vec::with_capacity(targets),
            is_otherwise: false,
            otherwise: None,
        }
    }

    pub fn contains(&self, local: Local) -> bool {
        self.locals.contains(&local)
    }

    pub fn add_variant(&mut self, value: Option<u128>) {
        if let Some(value) = value {
            self.variants.push(Variant::new(value));
            self.is_otherwise = false;
        } else {
            self.otherwise = Some(Default::default());
            self.is_otherwise = true;
        }
    }

    pub fn add_variant_item(&mut self, local: Local, item: Item<'tcx>) {
        assert!(self.locals.contains(&local));
        if self.is_otherwise {
            self.otherwise.as_mut().unwrap().insert(local, item);
        } else {
            self.variants.last_mut().unwrap().locals.insert(local, item);
        }
    }

    fn variants_len(&self) -> usize {
        self.variants.len() + self.otherwise.is_some() as usize
    }

    pub fn variants(&self) -> impl Iterator<Item = &Variant<'tcx>> {
        assert_eq!(self.targets, self.variants_len());
        self.variants.iter().map(|variant| {
            assert_eq!(self.locals.len(), variant.locals.len());
            for local in &self.locals {
                assert!(variant.locals.contains_key(local));
            }
            variant
        })
    }

    pub fn otherwise(&self) -> Option<&BTreeMap<Local, Item<'tcx>>> {
        let otherwise = self.otherwise.as_ref()?;
        assert_eq!(self.locals.len(), otherwise.len());
        for local in &self.locals {
            assert!(otherwise.contains_key(local));
        }

        self.otherwise.as_ref()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
struct BasicBlockWrap(BasicBlock);

impl Default for BasicBlockWrap {
    fn default() -> Self {
        Self(START_BLOCK)
    }
}

impl From<NodeIndex<BasicBlockWrap>> for BasicBlockWrap {
    fn from(idx: NodeIndex<BasicBlockWrap>) -> Self {
        BasicBlockWrap::new(idx.index())
    }
}

unsafe impl IndexType for BasicBlockWrap {
    fn new(x: usize) -> Self {
        Self(BasicBlock::from_usize(x))
    }

    fn index(&self) -> usize {
        self.0.index()
    }

    fn max() -> Self {
        Self(BasicBlock::MAX)
    }
}

#[derive(Debug, Clone)]
pub struct Context<'tcx> {
    pub generic_args: GenericArgsRef<'tcx>,
    pub module_id: ModuleId,
    pub locals: Locals<'tcx>,
    pub mir: &'tcx Body<'tcx>,
    pub fn_did: DefId,
    checked: FxHashSet<Local>,
    consts: FxHashMap<MirConst<'tcx>, Item<'tcx>>,
    switches: Vec<Switch<'tcx>>,
    post_dominators: Option<Dominators<NodeIndex<BasicBlockWrap>>>,
}

impl<'tcx> Context<'tcx> {
    pub fn new(
        fn_did: DefId,
        generic_args: GenericArgsRef<'tcx>,
        module_id: ModuleId,
        mir: &'tcx Body<'tcx>,
    ) -> Self {
        Self {
            generic_args,
            module_id,
            locals: Default::default(),
            mir,
            fn_did,
            checked: Default::default(),
            consts: Default::default(),
            switches: Default::default(),
            post_dominators: None,
        }
    }

    pub fn instantiate<T: TypeFoldable<TyCtxt<'tcx>>>(
        &self,
        tcx: TyCtxt<'tcx>,
        foldable: T,
    ) -> T {
        let binder = EarlyBinder::bind(foldable);
        binder.instantiate(tcx, self.generic_args)
    }

    #[inline]
    pub fn add_checked(&mut self, local: Local) {
        self.checked.insert(local);
    }

    #[inline]
    pub fn is_checked(&self, local: Local) -> bool {
        self.checked.contains(&local)
    }

    pub fn add_const(&mut self, const_: MirConst<'tcx>, item: Item<'tcx>) {
        self.consts.insert(const_, item);
    }

    pub fn find_const(&self, const_: &MirConst<'tcx>) -> Option<&Item<'tcx>> {
        self.consts.get(const_)
    }

    pub fn push_switch(&mut self, switch: Switch<'tcx>) {
        self.switches.push(switch)
    }

    pub fn pop_switch(&mut self) -> Option<Switch<'tcx>> {
        self.switches.pop()
    }

    pub fn last_switch(&mut self) -> Option<&mut Switch<'tcx>> {
        self.switches.last_mut()
    }

    pub fn immediate_post_dominator(
        &mut self,
        basic_blocks: &BasicBlocks,
        block: BasicBlock,
    ) -> Option<BasicBlock> {
        if self.post_dominators.is_none() {
            let (root, graph) = Self::create_rev_graph(basic_blocks);
            let dominators = simple_fast(&graph, root);
            self.post_dominators = Some(dominators)
        }

        self.post_dominators
            .as_ref()
            .unwrap()
            .immediate_dominator(NodeIndex::<_>::from(BasicBlockWrap(block)))
            .map(BasicBlockWrap::from)
            .map(|wrap| wrap.0)
    }

    fn create_rev_graph(
        basic_blocks: &BasicBlocks,
    ) -> (
        NodeIndex<BasicBlockWrap>,
        Graph<(), (), Directed, BasicBlockWrap>,
    ) {
        let mut graph = Graph::default();
        let mut root = None;

        for (block, block_data) in basic_blocks.iter_enumerated() {
            if let TerminatorKind::Return = block_data.terminator().kind {
                assert!(root.is_none());
                root = Some(NodeIndex::<_>::from(BasicBlockWrap(block)));
            }
            let idx = graph.add_node(());
            let idx = BasicBlockWrap::from(idx);
            assert_eq!(idx.0, block);
        }

        for (block, _) in basic_blocks.iter_enumerated() {
            let to: NodeIndex<_> = BasicBlockWrap(block).into();
            for successor in basic_blocks.successors(block) {
                let from: NodeIndex<_> = BasicBlockWrap(successor).into();
                graph.add_edge(from, to, ());
            }
        }

        (root.unwrap(), graph)
    }
}
