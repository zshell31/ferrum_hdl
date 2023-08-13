use std::fmt::{Arguments, Write};

use crate::net_list::Symbol;

#[derive(Debug, Clone)]
pub struct Ident(pub Symbol);

#[derive(Debug)]
pub struct Module {
    pub ident: Ident,
    pub parameters: Vec<Parameter>,
    pub block: Block,
}

impl Module {
    pub fn add_statement(&mut self, stmt: Statement) {
        self.block.0.push(stmt);
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub ident: Ident,
    pub kind: ParameterKind,
    pub width: u128,
}

#[derive(Debug, Clone, Copy)]
pub enum ParameterKind {
    Input,
    Output(NetKind),
}

#[derive(Debug, Clone, Copy)]
pub enum NetKind {
    Wire,
    Reg,
}

#[derive(Debug, Default)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug)]
pub enum Statement {
    Local(Local),
    Assignment(Ident, Expression),
    AlwaysBlock(SensitivityList, Block),
}

#[derive(Debug)]
pub struct Local {
    pub net_kind: NetKind,
    pub ident: Ident,
    pub width: u128,
}

#[derive(Debug, Default)]
pub struct SensitivityList(pub Vec<SensitivityParam>);

#[derive(Debug, Clone, Copy)]
pub enum Sensitivity {
    Empty,
    Posedge,
    Negedge,
}

#[derive(Debug)]
pub struct SensitivityParam {
    sensitivity: Sensitivity,
    ident: Ident,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Signal(Ident),
    Literal(Literal),
    Binary(BinOp, Box<Expression>, Box<Expression>),
    Unary(UnOp, Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub val: u128,
    pub width: u128,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    BitAnd,
    BitOr,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    BitNot,
    Not,
    Neg,
}

pub trait Visitor {
    fn visit_module(&mut self, module: &Module);

    fn visit_parameter(&mut self, parameter: &Parameter);

    fn visit_block(&mut self, block: &Block);

    fn visit_stmt(&mut self, stmt: &Statement);

    fn visit_sensitivity_param(&mut self, param: &SensitivityParam);

    fn visit_expr(&mut self, expr: &Expression);

    fn visit_bin_op(&mut self, bin_op: BinOp);

    fn visit_un_op(&mut self, un_op: UnOp);

    fn visit_local(&mut self, local: &Local);

    fn visit_net_kind(&mut self, net_kind: NetKind);

    fn visit_width(&mut self, width: u128);

    fn visit_ident(&mut self, ident: &Ident);

    fn visit_literal(&mut self, lit: &Literal);
}

pub trait Backend: Sized + Visitor {
    fn make() -> Self;

    fn generated(self) -> String;

    fn generate(module: &Module) -> String {
        let mut back = Self::make();
        back.visit_module(module);
        back.generated()
    }

    fn dir() -> &'static str;

    fn ext() -> &'static str;
}

#[derive(Debug)]
pub struct VerilogBackend {
    buffer: String,
    ident: usize,
    non_blocking: bool,
}

impl VerilogBackend {
    fn new() -> Self {
        Self {
            buffer: String::new(),
            ident: 0,
            non_blocking: false,
        }
    }

    fn write_char(&mut self, c: char) {
        self.buffer.write_char(c).unwrap()
    }

    fn write_str(&mut self, s: impl AsRef<str>) {
        self.buffer.write_str(s.as_ref()).unwrap()
    }

    fn write_fmt(&mut self, args: Arguments<'_>) {
        self.buffer.write_fmt(args).unwrap()
    }

    fn write_eol(&mut self) {
        self.buffer.write_char('\n').unwrap()
    }

    fn write_ident(&mut self) {
        for _ in 0 .. self.ident {
            self.buffer.write_str(TAB).unwrap();
        }
    }

    fn push_ident(&mut self) {
        self.ident += 1;
    }

    fn pop_ident(&mut self) {
        if self.ident > 0 {
            self.ident -= 1;
        }
    }
}

impl Backend for VerilogBackend {
    fn make() -> Self {
        Self::new()
    }

    fn generated(self) -> String {
        self.buffer
    }

    fn dir() -> &'static str {
        "verilog"
    }

    fn ext() -> &'static str {
        "v"
    }
}

const TAB: &str = "    ";

impl Visitor for VerilogBackend {
    fn visit_module(&mut self, module: &Module) {
        self.write_str("module ");
        self.visit_ident(&module.ident);

        if module.parameters.is_empty() {
            self.write_str("();");
            self.write_eol();
        } else {
            let mut first_param = true;

            self.push_ident();
            for param in &module.parameters {
                self.write_eol();
                self.write_ident();

                if first_param {
                    self.write_str("( ");
                    first_param = false;
                } else {
                    self.write_str(", ");
                }
                self.visit_parameter(param);
            }

            self.write_eol();
            self.write_ident();
            self.write_str(");");
            self.write_eol();

            self.pop_ident();
        };

        self.write_eol();
        self.visit_block(&module.block);
        self.write_eol();

        self.write_str("endmodule");
    }

    fn visit_parameter(&mut self, param: &Parameter) {
        match param.kind {
            ParameterKind::Input => {
                self.write_str("input wire ");
            }
            ParameterKind::Output(net_kind) => {
                self.write_str("output ");
                self.visit_net_kind(net_kind);
                self.write_char(' ');
            }
        }

        self.visit_width(param.width);

        if param.width > 1 {
            self.write_fmt(format_args!("[{}:0] ", param.width - 1));
        }

        self.visit_ident(&param.ident);
    }

    fn visit_block(&mut self, block: &Block) {
        self.push_ident();

        for stmt in &block.0 {
            self.visit_stmt(stmt);
        }

        self.pop_ident();
    }

    fn visit_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Local(local) => {
                self.visit_local(local);
            }
            Statement::Assignment(ident, expr) => {
                self.write_ident();

                if !self.non_blocking {
                    self.write_str("assign ");
                    self.visit_ident(ident);
                    self.write_str(" = ");
                    self.visit_expr(expr);
                } else {
                    self.visit_ident(ident);
                    self.write_str(" <= ");
                    self.visit_expr(expr);
                }
            }
            Statement::AlwaysBlock(sensitivity_list, block) => {
                self.write_ident();

                self.write_str("always @ (");
                let mut first_param = true;
                for param in &sensitivity_list.0 {
                    if first_param {
                        first_param = false;
                    } else {
                        self.write_str(" or ");
                    }

                    self.visit_sensitivity_param(param);
                }
                self.write_str(") begin");
                self.write_eol();

                self.non_blocking = true;
                self.visit_block(block);
                self.non_blocking = false;

                self.write_str("end");
            }
        };
        self.write_eol();
    }

    fn visit_sensitivity_param(&mut self, param: &SensitivityParam) {
        match param.sensitivity {
            Sensitivity::Empty => {}
            Sensitivity::Posedge => {
                self.write_str("posedge ");
            }
            Sensitivity::Negedge => {
                self.write_str("negedge ");
            }
        };

        self.visit_ident(&param.ident);
    }

    fn visit_expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Signal(ident) => {
                self.visit_ident(ident);
            }
            Expression::Literal(lit) => self.visit_literal(lit),
            Expression::Binary(op, left, right) => {
                self.write_char('(');

                self.visit_expr(left);
                self.write_char(' ');
                self.visit_bin_op(*op);
                self.write_char(' ');
                self.visit_expr(right);

                self.write_char(')');
            }
            Expression::Unary(op, expr) => {
                self.write_char('(');

                self.visit_un_op(*op);
                self.visit_expr(expr);

                self.write_char(')');
            }
        }
    }

    fn visit_bin_op(&mut self, bin_op: BinOp) {
        self.write_str(match bin_op {
            BinOp::BitAnd => "&",
            BinOp::BitOr => "|",
        })
    }

    fn visit_un_op(&mut self, un_op: UnOp) {
        self.write_str(match un_op {
            UnOp::BitNot => "~",
            UnOp::Not => "!",
            UnOp::Neg => "-",
        })
    }

    fn visit_local(&mut self, local: &Local) {
        self.visit_net_kind(local.net_kind);
        self.write_char(' ');
        self.visit_width(local.width);
        self.write_char(' ');
        self.visit_ident(&local.ident);
    }

    fn visit_net_kind(&mut self, net_kind: NetKind) {
        match net_kind {
            NetKind::Wire => {
                self.write_str("wire");
            }
            NetKind::Reg => {
                self.write_str("reg");
            }
        }
    }

    fn visit_width(&mut self, width: u128) {
        if width > 1 {
            self.write_fmt(format_args!("[{}:0]", width - 1));
        }
    }

    fn visit_ident(&mut self, _ident: &Ident) {
        todo!()
        // self.write_str(&ident.0);
    }

    fn visit_literal(&mut self, lit: &Literal) {
        self.write_fmt(format_args!("{}", lit.val));
    }
}
