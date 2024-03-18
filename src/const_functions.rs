pub use fhdl_const_func::*;

pub const fn slice_len(n: usize, m: usize) -> usize {
    assert!(m > 0);
    assert!(m <= n);
    n + 1 - m
}
