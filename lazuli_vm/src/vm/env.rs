use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::object;

pub type EnvRef = Rc<RefCell<Env>>;

#[derive(Default)]
pub struct Env {
    syms: HashMap<String, object::SymbolRef>,
    parent: Option<EnvRef>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            syms: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: EnvRef) -> Self {
        Env {
            syms: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn into_ref(self) -> EnvRef {
        Rc::new(RefCell::new(self))
    }

    pub fn set_symbol(&mut self, sym: object::SymbolRef) {
        let sym_name = {
            let sym_ref = sym.borrow();
            sym_ref.name().to_owned()
        };
        // If the symbol is currently defined, update its value
        if self.contains(&sym_name) {
            self.syms.insert(sym_name, sym);
            return;
        }

        // Otherwise, set it in the parent if one exists
        if let Some(p) = &self.parent {
            p.borrow_mut().set_symbol(sym);
            return;
        }

        // If global scope, set it anyway
        self.syms.insert(sym_name, sym);
    }

    pub fn set_global_symbol(&mut self, sym: object::SymbolRef) {
        if let Some(p) = &self.parent {
            p.borrow_mut().set_symbol(sym);
            return;
        }

        self.set_local_symbol(sym);
    }

    pub fn set_local_symbol(&mut self, sym: object::SymbolRef) {
        let sym_name = {
            let sym_ref = sym.borrow();
            sym_ref.name().to_owned()
        };
        self.syms.insert(sym_name, sym);
    }

    pub fn contains(&mut self, name: &str) -> bool {
        self.syms.contains_key(name)
    }

    pub fn get_symbol(&self, name: &str) -> object::SymbolRef {
        if let Some(sym) = self.syms.get(name) {
            return sym.clone();
        }

        if let Some(p) = &self.parent {
            p.borrow().get_symbol(name)
        } else {
            object::Symbol::new(name).into_ref()
        }
    }
}

impl ::std::fmt::Debug for Env {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        for sym in self.syms.values() {
            let sym_ref = sym.borrow();
            writeln!(f, "{} = {:?}", sym_ref.name(), sym_ref)?
        }

        if let Some(p) = &self.parent {
            write!(f, "Parent:\n{:?}", p.borrow())
        } else {
            Ok(())
        }
    }
}
