use rustc_hir::def_id::DefId;
use rustc_middle::ty::{AssocItem, AssocItems, AssocKind};

pub enum TreeNode<L, N> {
    Leaf(L),
    Node(N),
}

pub struct TreeIter<I> {
    stack: Vec<I>,
    size: usize,
}

impl<I> TreeIter<I> {
    pub fn new<T>(seed: T, size: usize) -> Self
    where
        T: IntoIterator<IntoIter = I>,
    {
        Self {
            stack: vec![seed.into_iter()],
            size,
        }
    }
}

impl<L, N, I> Iterator for TreeIter<I>
where
    I: Iterator<Item = TreeNode<L, N>>,
    N: IntoIterator<IntoIter = I>,
{
    type Item = L;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(node) = self.stack.last_mut() {
            let item = node.next();
            match item {
                Some(next) => match next {
                    TreeNode::Leaf(leaf) => {
                        return Some(leaf);
                    }
                    TreeNode::Node(node) => {
                        self.stack.push(node.into_iter());
                    }
                },
                None => {
                    self.stack.pop();
                }
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.size, Some(self.size))
    }
}

// https://stackoverflow.com/a/61452150
pub trait Captures<'tcx> {}
impl<'tcx, T: ?Sized> Captures<'tcx> for T {}

pub trait AssocItemsExt {
    fn find_by_lang_item(&self, lang_item: DefId, kind: AssocKind) -> Option<&AssocItem>;
}

impl AssocItemsExt for AssocItems {
    fn find_by_lang_item(&self, lang_item: DefId, kind: AssocKind) -> Option<&AssocItem> {
        self.in_definition_order()
            .find(|item| item.trait_item_def_id == Some(lang_item) && item.kind == kind)
    }
}
