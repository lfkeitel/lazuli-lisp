pub mod cons_list;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::str::FromStr;

use crate::vm::VM;
use cons_list::ConsList;

pub type Program = ConsList<Node>;
pub type BuiltinFn = fn(&mut VM, ConsList<Node>) -> Result<Node, String>;

impl ::std::fmt::Debug for Program {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        let mut s = String::new();
        self.iter()
            .for_each(|item| s.push_str(&format!("{} ", item)));
        write!(f, "({})", s.trim_end())
    }
}

// pub type NodeRef = Rc<RefCell<Node>>;

#[derive(Clone)]
pub enum Node {
    Symbol(SymbolRef),
    Number(i64),
    String(String),
    List(ConsList<Node>),
    Function(Function),
}

impl Node {
    pub fn empty_list() -> Node {
        Node::List(ConsList::new())
    }

    pub fn type_str(&self) -> &str {
        match self {
            Node::Symbol(_) => "Symbol",
            Node::Number(_) => "Number",
            Node::String(_) => "String",
            Node::List(_) => "List",
            Node::Function(_) => "Function",
        }
    }
}

impl FromStr for Node {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Node::String(s.to_owned()))
    }
}

impl ::std::fmt::Display for Node {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Node::Symbol(v) => write!(f, "{}", v.borrow()),
            Node::Number(v) => write!(f, "{}", v),
            Node::String(v) => write!(f, "\"{}\"", v),
            Node::List(v) => {
                let mut s = String::new();
                v.iter().for_each(|item| s.push_str(&format!("{} ", item)));
                write!(f, "({})", s.trim_end())
            }
            Node::Function(_) => write!(f, "function"),
        }
    }
}

impl ::std::fmt::Debug for Node {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", self)
    }
}

type SymbolProps = HashMap<Symbol, Node>;
pub type SymbolRef = Rc<RefCell<Symbol>>;

pub fn symbolref_to_node(sym: SymbolRef) -> Node {
    Node::Symbol(sym)
}

pub fn str_to_symbol_name(s: &str) -> String {
    s.to_uppercase().to_owned()
}

pub struct Symbol {
    name: String,
    pub value: Option<Node>, // Used when this symbol is evaulated outside a callable context
    pub function: Option<Callable>, // Used when this symbol is evaluated as a callable object
    properties: Option<SymbolProps>, // Only created when needed
}

impl Symbol {
    pub fn new(name: &str) -> Self {
        Symbol {
            name: str_to_symbol_name(&name),
            value: None,
            function: None,
            properties: None,
        }
    }

    pub fn new_with_builtin(name: &str, func: BuiltinFn) -> Self {
        Symbol {
            name: str_to_symbol_name(&name),
            value: None,
            function: Some(Callable::Builtin(func)),
            properties: None,
        }
    }

    pub fn into_ref(self) -> SymbolRef {
        Rc::new(RefCell::new(self))
    }

    pub fn into_node(self) -> Node {
        Node::Symbol(self.into_ref())
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn value(&self) -> Node {
        if let Some(val) = &self.value {
            val.clone()
        } else {
            Node::String(self.name.clone())
        }
    }
}

impl ::std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match &self.value() {
            Node::String(s) => write!(f, "{}", s),
            n => write!(f, "{}", n),
        }
    }
}

impl ::std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone)]
pub struct Function {
    is_macro: bool,
    params: Vec<String>,
    body: ConsList<Node>,
}

pub enum Callable {
    Builtin(BuiltinFn),
    User(Function),
}
