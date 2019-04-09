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

thread_local! {
    pub static TRUE_KW: Node = Node::new_keyword("t");
    pub static FALSE_KW: Node = Node::new_keyword("f"); // Or rather, falsey
}

#[derive(Clone)]
pub enum Node {
    Symbol(SymbolRef),
    Keyword(String),
    Number(i64),
    String(String),
    List(ConsList<Node>),
    Function(Callable),
}

impl Node {
    pub fn empty_list() -> Node {
        Node::List(ConsList::new())
    }

    pub fn type_str(&self) -> &str {
        match self {
            Node::Symbol(_) => "Symbol",
            Node::Keyword(_) => "Keyword",
            Node::Number(_) => "Number",
            Node::String(_) => "String",
            Node::List(_) => "List",
            Node::Function(_) => "Function",
        }
    }

    pub fn new_keyword(name: &str) -> Self {
        Node::Keyword(str_to_symbol_name(name))
    }

    pub fn bool_obj(b: bool) -> Node {
        if b {
            TRUE_KW.with(|t| t.clone())
        } else {
            FALSE_KW.with(|f| f.clone())
        }
    }

    pub fn is_truthy(&self) -> bool {
        TRUE_KW.with(|t| self == t)
    }
}

impl PartialEq for Node {
    // Can't derive this since Symbol contains a SymbolRef and you can't
    // implement external traits on an external type (Rc<T>)

    fn eq(&self, other: &Node) -> bool {
        match (self, other) {
            (Node::Symbol(v1), Node::Symbol(v2)) => v1.borrow().name == v2.borrow().name,
            (Node::Keyword(v1), Node::Keyword(v2)) => v1 == v2,
            (Node::Number(v1), Node::Number(v2)) => v1 == v2,
            (Node::String(v1), Node::String(v2)) => v1 == v2,
            (Node::Function(v1), Node::Function(v2)) => v1 == v2,
            (Node::List(v1), Node::List(v2)) => v1 == v2,
            _ => false,
        }
    }
}

impl Default for Node {
    fn default() -> Self {
        FALSE_KW.with(|f| f.clone())
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
            Node::Keyword(v) => write!(f, "{}", v),
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

#[derive(Clone, PartialEq)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Box<Node>,
}

#[derive(Clone)]
pub enum Callable {
    Builtin(BuiltinFn),
    Func(Function),
    Macro(Function),
}

impl Callable {
    pub fn into_macro(self) -> Self {
        match self {
            Callable::Macro(_) => self,
            Callable::Func(f) => Callable::Macro(f),
            Callable::Builtin(_) => panic!("Cannot make builtin func into a macro"),
        }
    }
}

impl PartialEq for Callable {
    fn eq(&self, other: &Callable) -> bool {
        match (self, other) {
            (Callable::Builtin(v1), Callable::Builtin(v2)) => {
                v1 as *const BuiltinFn as usize == v2 as *const BuiltinFn as usize
            }
            (Callable::Macro(v1), Callable::Macro(v2)) => v1 == v2,
            (Callable::Func(v1), Callable::Func(v2)) => v1 == v2,
            _ => false,
        }
    }
}
