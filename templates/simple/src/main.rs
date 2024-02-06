use ferrum_hdl::prelude::*;
use {{project_name}}::top_module;

fn main() {
    let clk = Clock::default();
    let rst = Reset::reset();

    let mut sim = top_module(clk, &rst).simulate();
    for value in sim.take(8) {
        println!("value = {:?}", value);
    }
}
