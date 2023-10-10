# Ferrum HDL
The experimental HDL framework for writing FPGA firmware using the Rust Programming Language. The framework is inspired by [Clash](https://clash-lang.org/) 
and allows to both simulate and synthesize verilog code. Rust code during synthesizing is transformed into the graph consisting of 
nodes of different kinds (Pass, Const, BinOp, DFF, etc) and the graph is independent of the target HDL language. So theoretically it can be to add different targets for synthesizing 
(Verilog, VHDL, Yosys RTLIL, or for example to export into the internal format used by [Turing Complete](https://turingcomplete.game/)). Currently, Verilog is supported.

# Example
[Led shifter demo](https://github.com/zshell31/led_shifter)

# How to synthesize
Currently, there is no ready-made cargo cli to synthesize verilog code from rust code (I'm planning to add it in the future).
Instead, to synthesize verilog code, run the command:
```
RUST_BACKTRACE=1 RUSTC_WRAPPER=<path>/ferrum_hdl/target/debug/fhdl_compiler cargo build --lib
```

Rust code for synthesizing should be organized as a lib crate that exports the public function `top_module`. 
The result of synthesis is placed into `<crate root>/generated/verilog/top_module.v`.
