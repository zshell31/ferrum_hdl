use std::{
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use ferrum_hdl::const_functions::clog2;
use fhdl_netlist::{group::ItemId, net_list::ModuleId, sig_ty::SignalTy};
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
use rustc_index::IndexVec;
use rustc_middle::{
    mir::{
        BasicBlock, BasicBlocks, Body, Const as MirConst, Local, Operand, TerminatorKind,
        START_BLOCK,
    },
    ty::{EarlyBinder, GenericArgsRef, TyCtxt},
};
use rustc_span::Span;
use rustc_target::abi::FieldIdx;
use rustc_type_ir::fold::TypeFoldable;

use crate::error::{Error, SpanError, SpanErrorKind};

#[derive(Debug, Clone, Copy)]
pub enum ModuleOrItem {
    Module(ModuleId),
    Item(ItemId),
}

impl ModuleOrItem {
    pub fn mod_id(self) -> ModuleId {
        match self {
            Self::Module(mod_id) => mod_id,
            _ => panic!("expected module id, not item id"),
        }
    }

    pub fn item_id(self) -> ItemId {
        match self {
            Self::Item(item_id) => item_id,
            _ => panic!("exptected item id, not module id"),
        }
    }
}

impl From<ModuleId> for ModuleOrItem {
    fn from(mod_id: ModuleId) -> Self {
        Self::Module(mod_id)
    }
}

impl From<ItemId> for ModuleOrItem {
    fn from(item_id: ItemId) -> Self {
        Self::Item(item_id)
    }
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub value: u128,
    pub locals: BTreeMap<Local, ItemId>,
}

impl Variant {
    fn new(value: u128) -> Self {
        Self {
            value,
            locals: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Switch {
    pub locals: BTreeSet<Local>,
    pub discr: ItemId,
    pub width: u128,
    targets: usize,
    variants: Vec<Variant>,
    is_otherwise: bool,
    otherwise: Option<BTreeMap<Local, ItemId>>,
}

impl Switch {
    pub fn new(locals: FxHashSet<Local>, discr: ItemId, targets: usize) -> Self {
        Self {
            locals: locals.into_iter().collect(),
            discr,
            targets,
            variants: Vec::with_capacity(targets),
            width: 0,
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
            let width = clog2(value as usize) as u128;
            if width > self.width {
                self.width = width;
            }
            self.is_otherwise = false;
        } else {
            self.otherwise = Some(Default::default());
            self.is_otherwise = true;
        }
    }

    pub fn add_variant_item_id(&mut self, local: Local, item_id: ItemId) {
        assert!(self.locals.contains(&local));
        if self.is_otherwise {
            self.otherwise.as_mut().unwrap().insert(local, item_id);
        } else {
            self.variants
                .last_mut()
                .unwrap()
                .locals
                .insert(local, item_id);
        }
    }

    fn variants_len(&self) -> usize {
        self.variants.len() + self.otherwise.is_some() as usize
    }

    pub fn variants(&self) -> impl Iterator<Item = &Variant> {
        assert_eq!(self.targets, self.variants_len());
        self.variants.iter().map(|variant| {
            assert_eq!(self.locals.len(), variant.locals.len());
            for local in &self.locals {
                assert!(variant.locals.contains_key(local));
            }
            variant
        })
    }

    pub fn otherwise(&self) -> Option<&BTreeMap<Local, ItemId>> {
        let otherwise = self.otherwise.as_ref()?;
        assert_eq!(self.locals.len(), otherwise.len());
        for local in &self.locals {
            assert!(otherwise.contains_key(local));
        }

        self.otherwise.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct Closure<'tcx> {
    pub upvars: IndexVec<FieldIdx, Operand<'tcx>>,
    pub output_ty: SignalTy,
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
pub struct EvalContext<'tcx> {
    pub is_primary: bool,
    pub generic_args: GenericArgsRef<'tcx>,
    pub module_id: ModuleId,
    pub locals: FxHashMap<Local, ModuleOrItem>,
    pub mir: &'tcx Body<'tcx>,
    pub fn_did: DefId,
    checked: FxHashSet<Local>,
    consts: FxHashMap<MirConst<'tcx>, ItemId>,
    switches: Vec<Switch>,
    closures: FxHashMap<ModuleId, Rc<Closure<'tcx>>>,
    post_dominators: Option<Dominators<NodeIndex<BasicBlockWrap>>>,
}

impl<'tcx> EvalContext<'tcx> {
    pub fn new(
        is_primary: bool,
        fn_did: DefId,
        generic_args: GenericArgsRef<'tcx>,
        module_id: ModuleId,
        mir: &'tcx Body<'tcx>,
    ) -> Self {
        Self {
            is_primary,
            generic_args,
            module_id,
            locals: Default::default(),
            mir,
            fn_did,
            checked: Default::default(),
            consts: Default::default(),
            switches: Default::default(),
            closures: Default::default(),
            post_dominators: None,
        }
    }

    pub fn instantiate<T: TypeFoldable<TyCtxt<'tcx>>>(
        &self,
        tcx: TyCtxt<'tcx>,
        foldable: T,
    ) -> T {
        let binder = EarlyBinder::bind(foldable);
        self.instantiate_early_binder(tcx, binder)
    }

    pub fn instantiate_early_binder<T: TypeFoldable<TyCtxt<'tcx>>>(
        &self,
        tcx: TyCtxt<'tcx>,
        binder: EarlyBinder<T>,
    ) -> T {
        if self.is_primary {
            binder.instantiate(tcx, self.generic_args)
        } else {
            binder.instantiate_identity()
        }
    }

    pub fn with_generic_args(&self, generic_args: GenericArgsRef<'tcx>) -> Self {
        Self {
            is_primary: self.is_primary,
            generic_args,
            module_id: self.module_id,
            mir: self.mir,
            fn_did: self.fn_did,
            checked: Default::default(),
            locals: Default::default(),
            consts: Default::default(),
            switches: Default::default(),
            closures: Default::default(),
            post_dominators: None,
        }
    }

    pub fn with_module_id(&self, module_id: ModuleId) -> Self {
        Self {
            is_primary: self.is_primary,
            generic_args: self.generic_args,
            module_id,
            mir: self.mir,
            fn_did: self.fn_did,
            checked: Default::default(),
            locals: Default::default(),
            consts: Default::default(),
            switches: Default::default(),
            closures: Default::default(),
            post_dominators: None,
        }
    }

    pub fn add_local(&mut self, local: Local, mod_or_item: impl Into<ModuleOrItem>) {
        self.locals.insert(local, mod_or_item.into());
    }

    pub fn find_local(&self, local: Local, span: Span) -> Result<ModuleOrItem, Error> {
        self.locals.get(&local).copied().ok_or_else(|| {
            SpanError::new(SpanErrorKind::MissingLocal(local), span).into()
        })
    }

    #[inline]
    pub fn add_checked(&mut self, local: Local) {
        self.checked.insert(local);
    }

    #[inline]
    pub fn is_checked(&self, local: Local) -> bool {
        self.checked.contains(&local)
    }

    pub fn add_const(&mut self, const_: MirConst<'tcx>, item_id: ItemId) {
        self.consts.insert(const_, item_id);
    }

    pub fn find_const(&self, const_: &MirConst<'tcx>) -> Option<ItemId> {
        self.consts.get(const_).copied()
    }

    pub fn push_switch(&mut self, switch: Switch) {
        self.switches.push(switch)
    }

    pub fn pop_switch(&mut self) -> Option<Switch> {
        self.switches.pop()
    }

    pub fn last_switch(&mut self) -> Option<&mut Switch> {
        self.switches.last_mut()
    }

    pub fn add_closure(&mut self, module_id: ModuleId, closure: Closure<'tcx>) {
        self.closures.insert(module_id, Rc::new(closure));
    }

    pub fn find_closure(
        &self,
        module_id: ModuleId,
        span: Span,
    ) -> Result<Rc<Closure<'tcx>>, Error> {
        self.closures
            .get(&module_id)
            .cloned()
            .ok_or_else(|| SpanError::new(SpanErrorKind::ExpectedClosure, span).into())
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
