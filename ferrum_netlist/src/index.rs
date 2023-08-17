#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeIndex(pub(crate) usize, pub(crate) u8);

pub trait Index {
    fn index(&mut self, out: u8) -> NodeIndex;
}
