use std::{
    collections::HashMap,
    io::{
        self,
        Write,
    },
};

pub enum Node {
    Text(String),
    Element(Element)
}

pub struct Element {
    name: String,
    attributes: HashMap<String, String>,
    children: Vec<Node>
}

impl Element {
    pub fn new(name: &str) -> Self {
        Self {
            name.to_string(),
            attributes: HashMap::new(),
            children: vec![]
        }
    }

    pub fn with_attribute(mut self, name: &str, value: &str) -> Self {
        self.attributes.insert(name.to_string(), value.to_string());
        self
    }

    pub fn with_child<T: Into<Element>>(mut self, node: T) -> Self {
        self.children.push(node.into());
        self
    }

    pub fn write_to<T: Write>(&self, dest: T) -> io::Result<()> {
        Ok(())
    }
}

impl From<Element> for Node {
    fn from(src: Element) -> Self {
        Node::Element(src)
    }
}

impl From<String> for Node {
    fn from(src: String) -> Self {
        Node::Text(src)
    }
}
