use ferrum::bit::Bit;
use ferrum_netlist::{
    net_list::NetList,
    node::{BitAndComp, BitNotComp, Const, DFFNode, Input, Mux2, Node, DFF},
    symbol::Symbol,
};

#[allow(dead_code)]
fn seq_logic() {
    let mut net_list = NetList::new(Symbol::new("top_module"));

    let (clk,) = net_list.add_component(Input::<Bit>::default(), (Symbol::new("clk"),));

    let (rst_value,) = net_list
        .add_component(Const::<Bit> { value: Bit::LOW }, (Symbol::new("rst_val"),));

    let (q,) = net_list.add_component(
        DFF::<Bit> {
            clk,
            rst_value,
            data: None,
        },
        (Symbol::new("q"),),
    );

    let (q1,) =
        net_list.add_component(BitNotComp::<Bit> { input: q }, (Symbol::new("q1"),));

    if let Node::DFF(DFFNode { data, .. }) = net_list.node_mut(q.index()) {
        *data = Some(q1.index());
    }

    net_list.add_output(q1);

    println!("{}", net_list.verilog());
}

fn seq_logic1() {
    let mut net_list = NetList::new(Symbol::new("top_module"));

    let (clk,) = net_list.add_component(Input::<Bit>::default(), (Symbol::new("clk"),));
    let (data,) = net_list.add_component(Input::<Bit>::default(), (Symbol::new("data"),));
    let (sel,) = net_list.add_component(Input::<Bit>::default(), (Symbol::new("sel"),));
    let (en,) = net_list.add_component(Input::<Bit>::default(), (Symbol::new("en"),));

    let (rst_value,) = net_list
        .add_component(Const::<Bit> { value: Bit::HIGH }, (Symbol::new("rst_val"),));

    let (dff_q,) = net_list.add_component(
        DFF::<Bit> {
            clk,
            rst_value,
            data: None,
        },
        (Symbol::new("q"),),
    );

    let (q,) =
        net_list.add_component(BitNotComp::<Bit> { input: dff_q }, (Symbol::new("q1"),));

    let (q,) = net_list.add_component(
        BitAndComp::<Bit> {
            input1: q,
            input2: en,
        },
        (Symbol::new("q2"),),
    );

    let (d,) = net_list.add_component(
        Mux2::<Bit> {
            sel,
            input1: q,
            input2: data,
        },
        (Symbol::new("d"),),
    );

    if let Node::DFF(DFFNode { data, .. }) = net_list.node_mut(dff_q.index()) {
        *data = Some(d.index());
    }

    net_list.add_output(q);

    println!("{}", net_list.verilog());
}

fn main() {
    seq_logic1();
}
