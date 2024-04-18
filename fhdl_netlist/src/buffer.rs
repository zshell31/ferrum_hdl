use std::{
    fmt::Arguments,
    io::{self, Result, Write},
};

pub struct Buffer<W> {
    pub inner: W,
    pub tab: u8,
}

const TAB: &str = "    ";

impl<W: Write> Buffer<W> {
    pub fn new(inner: W) -> Self {
        Self { inner, tab: 0 }
    }

    pub fn write_char(&mut self, c: char) -> Result<()> {
        let mut b = [0; 2];
        let b = c.encode_utf8(&mut b).as_bytes();

        self.inner.write_all(b)
    }

    pub fn write_str(&mut self, s: &str) -> Result<()> {
        self.inner.write_all(s.as_bytes())
    }

    pub fn write_n_str(&mut self, n: usize, s: &str) -> Result<()> {
        for _ in 0 .. n {
            self.write_str(s)?;
        }

        Ok(())
    }

    pub fn write_fmt(&mut self, args: Arguments<'_>) -> Result<()> {
        self.inner.write_fmt(args)
    }

    pub fn push_tab(&mut self) {
        self.tab += 1;
    }

    pub fn pop_tab(&mut self) {
        self.tab = self.tab.saturating_sub(1);
    }

    pub fn write_tab(&mut self) -> Result<()> {
        for _ in 0 .. self.tab {
            self.write_str(TAB)?;
        }

        Ok(())
    }

    #[inline]
    pub fn write_eol(&mut self) -> Result<()> {
        self.write_char('\n')
    }

    pub fn write_template(&mut self, template: Arguments<'_>) -> Result<()> {
        for line in template.to_string().trim().lines() {
            self.write_tab()?;
            self.write_str(line)?;
            self.write_eol()?;
        }

        Ok(())
    }

    pub fn intersperse<T>(
        &mut self,
        sep: &str,
        iter: impl IntoIterator<Item = T>,
        f: impl Fn(&mut Self, T) -> Result<()>,
    ) -> Result<()> {
        let mut peekable = iter.into_iter().peekable();
        while let Some(item) = peekable.next() {
            f(self, item)?;
            if peekable.peek().is_some() {
                self.write_str(sep)?;
            }
        }

        Ok(())
    }

    #[inline]
    pub fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}
