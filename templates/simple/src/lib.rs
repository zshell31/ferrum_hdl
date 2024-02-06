#![allow(incomplete_features)]
#![allow(clippy::let_and_return)]
#![feature(generic_const_exprs)]

pub use ferrum_hdl::prelude::*;

pub fn top_module(
    clk: Clock<TestSystem4>,
    rst: &Reset<TestSystem4>,
) -> Signal<TestSystem4, Unsigned<4>> {
    reg(clk, rst, |counter| counter + 1)
}
