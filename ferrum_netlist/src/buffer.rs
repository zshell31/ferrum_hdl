use std::fmt::{Arguments, Write};

#[derive(Default)]
pub struct Buffer {
    pub buffer: String,
    pub ident: u8,
}

const TAB: &str = "    ";

impl Buffer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_with_ident(ident: u8) -> Self {
        Self {
            ident,
            ..Default::default()
        }
    }

    pub fn write_char(&mut self, c: char) {
        self.buffer.write_char(c).unwrap();
    }

    pub fn write_str(&mut self, s: &str) {
        self.buffer.write_str(s).unwrap();
    }

    pub fn write_fmt(&mut self, args: Arguments<'_>) {
        self.buffer.write_fmt(args).unwrap();
    }

    pub fn push_ident(&mut self) {
        self.ident += 1;
    }

    pub fn pop_ident(&mut self) {
        if self.ident > 0 {
            self.ident -= 1;
        }
    }

    pub fn write_ident(&mut self) {
        for _ in 0 .. self.ident {
            self.write_str(TAB);
        }
    }

    pub fn write_eol(&mut self) {
        self.write_char('\n');
    }

    pub fn extend(&mut self, buffer: Buffer) {
        self.buffer.push_str(&buffer.buffer);
    }

    pub fn write_template(&mut self, template: Arguments<'_>) {
        for line in template.to_string().trim().lines() {
            self.write_ident();
            self.write_str(line);
            self.write_eol();
        }
        self.write_eol();
    }
}
