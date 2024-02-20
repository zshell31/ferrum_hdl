use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
};

#[derive(Debug, Default)]
pub struct Trie {
    children: Vec<Node>,
}

impl Display for Trie {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("root\n")?;
        if !self.children.is_empty() {
            let ident = 2;
            for node in &self.children {
                node.display(ident, f)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Node {
    path: &'static str,
    children: Vec<Node>,
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.path)
    }
}

impl Node {
    fn new(path: &'static str) -> Self {
        Self {
            path,
            children: Vec::new(),
        }
    }

    fn cmp(&self, other: &&str) -> Ordering {
        self.path.cmp(other)
    }

    fn display(&self, mut ident: usize, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:ident$}{}", " ", self.path)?;
        ident += 2;
        for node in &self.children {
            node.display(ident, f)?;
        }

        Ok(())
    }
}

impl Trie {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, path: &[&'static str]) {
        let mut children = &mut self.children;
        for path in path {
            let node = children.binary_search_by(|node| node.cmp(path));

            children = match node {
                Ok(idx) => &mut children[idx].children,
                Err(idx) => {
                    let node = Node::new(path);
                    children.insert(idx, node);
                    &mut children[idx].children
                }
            }
        }
    }

    pub fn find<T: AsRef<str>>(&self, path: impl IntoIterator<Item = T>) -> bool {
        let mut children = &self.children;
        for path in path {
            let node = children.binary_search_by(|node| node.cmp(&path.as_ref()));

            children = match node {
                Ok(idx) => &children[idx].children,
                Err(_) => {
                    return false;
                }
            }
        }

        true
    }
}
