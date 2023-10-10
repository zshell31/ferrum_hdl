use std::fmt::{Arguments, Write};

#[derive(Default)]
pub struct Buffer {
    pub buffer: String,
    pub tab: u8,
}

const TAB: &str = "    ";

impl Buffer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_with_ident(ident: u8) -> Self {
        Self {
            tab: ident,
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

    pub fn push_tab(&mut self) {
        self.tab += 1;
    }

    pub fn pop_tab(&mut self) {
        if self.tab > 0 {
            self.tab -= 1;
        }
    }

    pub fn write_tab(&mut self) {
        for _ in 0 .. self.tab {
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
            self.write_tab();
            self.write_str(line);
            self.write_eol();
        }
        self.write_eol();
    }

    pub fn intersperse<T>(
        &mut self,
        sep: &str,
        iter: impl IntoIterator<Item = T>,
        f: impl Fn(&mut Self, T),
    ) {
        let mut peekable = iter.into_iter().peekable();
        while let Some(item) = peekable.next() {
            f(self, item);
            if peekable.peek().is_some() {
                self.write_str(sep);
            }
        }
    }
}
