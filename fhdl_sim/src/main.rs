use ferrum_hdl::prelude::Bit;
use fhdl_sim::{Clock, Value};

#[allow(dead_code)]
pub struct TopModule {
    clk: Clock<"clk">,
    rst: Value<Bit, "rst">,
    en: Value<Bit, "en">,
}

fn main() {}
