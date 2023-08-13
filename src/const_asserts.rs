pub struct Assert<const C: bool>;

pub trait IsTrue {}
impl IsTrue for Assert<true> {}
