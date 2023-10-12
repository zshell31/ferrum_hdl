// TODO: Move into other crate
pub use fhdl_netlist::const_functions::*;

pub const fn bit(m: usize, n: usize) -> bool {
    m < n
}

pub const fn slice(s: usize, m: usize, n: usize) -> bool {
    m > 0 && s + m <= n
}
