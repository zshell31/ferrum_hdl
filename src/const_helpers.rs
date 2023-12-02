pub struct Assert<const C: bool>;

pub trait IsTrue {}
impl IsTrue for Assert<true> {}

pub struct EvalU128<const T: u128>;

impl<const T: u128> EvalU128<T> {
    pub const R: u128 = T;
}

pub struct ConstConstr<const N: usize>;
