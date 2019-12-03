mod builtins;
mod env;

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::object::cons_list::ConsList;
use crate::object::{self, Callable, Function, Node, Program, Symbol};

use crate::vm::builtins::arithmetic;
use crate::vm::builtins::import;
use crate::vm::builtins::lists;
use crate::vm::builtins::logic;
use crate::vm::builtins::quote;
use crate::vm::builtins::strings;

#[derive(Default)]
pub struct VM {
    pub symbols: env::EnvRef,
    cmd_not_found: Option<Callable>,
    filenames: Vec<PathBuf>,
}

macro_rules! make_builtin {
    ($vm:ident, $sym:expr, $func:expr) => {{
        let mut sym = object::Symbol::new($sym);
        sym.function = Some(object::Callable::Builtin($func));
        $vm.symbols.borrow_mut().set_symbol(sym.into_ref());
    }};
}

#[macro_export]
macro_rules! args_setup_error {
    (==) => {
        "{} expected {} args, got {}"
    };

    (>=) => {
        "{} expected at least {} args, got {}"
    };
}

#[macro_export]
macro_rules! args_setup {
    ($args_list:ident) => {
        {
            $args_list.iter().collect::<Vec<&Node>>()
        }
    };

    ($args_list:ident, $sym:expr, $checkl:expr, $checkh:expr) => {
        {
            let args: Vec<&Node> = $args_list.iter().collect();

            if !(args.len() >= $checkl && args.len() <= $checkh) {
                return Err(format!(
                    "{} expected at between {} and {} args, got {}",
                    $sym,
                    $checkl,
                    $checkh,
                    args.len()
                ))
            }

            args
        }
    };

    ($args_list:ident, $sym:expr, $oper:tt, $check:expr) => {
        {
            let args: Vec<&Node> = $args_list.iter().collect();

            if !(args.len() $oper $check) {
                return Err(format!(
                    $crate::args_setup_error!($oper),
                    $sym,
                    $check,
                    args.len()
                ))
            }

            args
        }
    };

    ($args_list:ident, $name:expr, $check:expr) => {
        $crate::args_setup!($args_list, $name, ==, $check)
    };
}

impl VM {
    pub fn new() -> Self {
        let vm = VM {
            symbols: env::Env::new().into_ref(),
            cmd_not_found: None,
            filenames: Vec::new(),
        };

        make_builtin!(vm, "include", import::include);
        make_builtin!(vm, "define", builtin_define);
        make_builtin!(vm, "define-syntax", builtin_defmacro);
        make_builtin!(vm, "setq", builtin_setq);
        make_builtin!(vm, "setf", builtin_setf);
        make_builtin!(vm, "print", builtin_print);
        make_builtin!(vm, "quote", quote::quote);
        make_builtin!(vm, "quasiquote", quote::quasiquote);
        make_builtin!(vm, "progn", builtin_progn);
        make_builtin!(vm, "lambda", builtin_lambda);
        make_builtin!(vm, "list", lists::make_list);
        make_builtin!(vm, "concat", lists::concat);
        make_builtin!(vm, "head", lists::head);
        make_builtin!(vm, "tail", lists::tail);
        make_builtin!(vm, "eval", builtin_eval);
        make_builtin!(vm, "loop", builtin_loop);
        make_builtin!(vm, "while", builtin_while);
        make_builtin!(vm, "parse-int", builtin_parse_int);
        make_builtin!(vm, "parse-float", builtin_parse_float);
        make_builtin!(
            vm,
            "debug-print-symbol-table",
            builtin_debug_print_symbol_table
        );
        make_builtin!(vm, "+", arithmetic::add);
        make_builtin!(vm, "-", arithmetic::sub);
        make_builtin!(vm, "*", arithmetic::mul);
        make_builtin!(vm, "/", arithmetic::div);
        make_builtin!(vm, "<", logic::logic_lt);
        make_builtin!(vm, ">", logic::logic_gt);
        make_builtin!(vm, "eq", logic::logic_eq);
        make_builtin!(vm, "not", logic::logic_not);
        make_builtin!(vm, "and", logic::logic_and);
        make_builtin!(vm, "or", logic::logic_or);
        make_builtin!(vm, "if", logic::logic_if);

        make_builtin!(vm, "make-map", builtin_make_map);

        make_builtin!(vm, "get-key", builtin_get_key);
        make_builtin!(vm, "set-key", builtin_set_key);
        make_builtin!(vm, "has-key", builtin_has_key);

        make_builtin!(vm, "expand-macro", builtin_expand_macro);

        make_builtin!(vm, "string-concat", strings::string_concat);
        make_builtin!(vm, "string-replace", strings::string_replace);
        make_builtin!(vm, "string-split", strings::string_split);
        make_builtin!(vm, "current-file", builtin_current_file);

        vm
    }

    pub fn with_env(&self, env: env::EnvRef) -> Self {
        VM {
            symbols: env,
            cmd_not_found: self.cmd_not_found.clone(),
            filenames: Vec::new(),
        }
    }

    pub fn set_cmd_not_found(&mut self, c: Callable) {
        self.cmd_not_found = Some(c);
    }

    pub fn add_symbol(&mut self, sym: object::SymbolRef) {
        self.symbols.borrow_mut().set_symbol(sym);
    }

    pub fn add_filename<P: AsRef<Path>>(&mut self, name: P) {
        self.filenames.push(name.as_ref().to_owned());
    }

    pub fn pop_filename(&mut self) -> Option<PathBuf> {
        self.filenames.pop()
    }

    pub fn current_filename(&mut self) -> Option<&PathBuf> {
        self.filenames.last()
    }

    pub fn run(&mut self, program: &Program) -> Result<Node, String> {
        // println!("{:?}", self.symbols);
        let mut ret = Node::empty_list();
        for form in program.iter() {
            ret = self.eval(&form)?
        }
        Ok(ret)
    }

    pub fn eval(&mut self, form: &Node) -> Result<Node, String> {
        match form {
            Node::Symbol(sym_ref) => {
                let sym = sym_ref.borrow();
                let sym_name = sym.name();
                Ok(self.symbols.borrow().get_symbol(&sym_name).borrow().value())
            }
            Node::Number(num) => Ok(Node::Number(*num)),
            Node::Float(num) => Ok(Node::Float(*num)),
            Node::String(string) => Ok(Node::String(string.to_owned())),
            Node::List(list) => self.eval_list(&list),
            Node::Keyword(v) => Ok(Node::Keyword(v.clone())),
            _ => Err("Not supported".to_owned()),
        }
    }

    pub fn eval_items(&mut self, items: &[&Node]) -> Result<Vec<Node>, String> {
        let mut results = Vec::with_capacity(items.len());

        for item in items {
            results.push(self.eval(item)?);
        }

        Ok(results)
    }

    fn eval_function(&mut self, func: &Callable, args: ConsList<Node>) -> Result<Node, String> {
        match func {
            Callable::Builtin(f) => f(self, args),
            Callable::Func(f) => {
                let mut new_env = env::Env::with_parent(self.symbols.clone());

                let args = match f.params.last() {
                    Some(s) => {
                        if s == "&rest" {
                            let mut args = args_setup!(args, "<func1>", >=, f.params.len()-1);
                            let rest = args.split_off(f.params.len() - 1);
                            let rest_list = Node::from_vec_ref(rest);

                            let mut sym = Symbol::new("rest");
                            sym.value = Some(rest_list);
                            new_env.set_local_symbol(sym.into_ref());

                            args
                        } else {
                            args_setup!(args, "<func2>", f.params.len())
                        }
                    }
                    None => args_setup!(args, "<func>", f.params.len()),
                };

                for (i, arg) in args.iter().enumerate() {
                    let mut sym = Symbol::new(&f.params[i]);
                    sym.value = Some(self.eval(arg)?);
                    new_env.set_local_symbol(sym.into_ref());
                }

                self.with_env(new_env.into_ref()).eval(&f.body)
            }
            Callable::Macro(f) => {
                let mut new_env = env::Env::with_parent(self.symbols.clone());

                let args = match f.params.last() {
                    Some(s) => {
                        if s == "&rest" {
                            let mut args = args_setup!(args, "<func1>", >=, f.params.len()-1);
                            let rest = args.split_off(f.params.len() - 1);
                            let rest_list = Node::from_vec_ref(rest);

                            let mut sym = Symbol::new("rest");
                            sym.value = Some(rest_list);
                            new_env.set_symbol(sym.into_ref());

                            args
                        } else {
                            args_setup!(args, "<func2>", f.params.len())
                        }
                    }
                    None => args_setup!(args, "<func>", f.params.len()),
                };

                for (i, arg) in args.iter().enumerate() {
                    let mut sym = Symbol::new(&f.params[i]);
                    sym.value = Some((*arg).clone());
                    new_env.set_symbol(sym.into_ref());
                }

                let expanded = self.with_env(new_env.into_ref()).eval(&f.body)?;
                self.eval(&expanded)
            }
        }
    }

    pub fn eval_list(&mut self, form: &ConsList<Node>) -> Result<Node, String> {
        let h = form.head();
        if h.is_none() {
            return Ok(Node::bool_obj(false));
        }

        let head = h.unwrap();
        if let Node::Symbol(sym_ref) = &head {
            let sym = {
                let sym_table_ref = self.symbols.borrow().get_symbol(sym_ref.borrow().name());
                let b = sym_table_ref.borrow();
                b.clone()
            };

            return if let Some(func) = &sym.function {
                self.eval_function(&func, form.tail())
            } else if let Some(c) = self.cmd_not_found.clone() {
                self.eval_function(&c, form.clone())
            } else {
                Err(format!("Undefined function {}", sym.name()))
            };
        }

        let evaled_head = self.eval(&head)?;

        match &evaled_head {
            Node::Symbol(sym_ref) => {
                let sym = {
                    let sym_table_ref = self.symbols.borrow().get_symbol(sym_ref.borrow().name());
                    let b = sym_table_ref.borrow();
                    b.clone()
                };

                if let Some(func) = &sym.function {
                    self.eval_function(&func, form.tail())
                } else if let Some(c) = self.cmd_not_found.clone() {
                    self.eval_function(&c, form.clone())
                } else {
                    Err(format!("Undefined function {}", sym.name()))
                }
            }
            Node::Function(func) => self.eval_function(&func, form.tail()),
            _ => Err(format!(
                "Cannot evaluate non-symbol object {}",
                head.type_str()
            )),
        }
    }
}

fn builtin_define(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    // Collect into a vector to make it easier to work with args
    let args = args_setup!(args_list, "define", 2);

    let arg1 = args[0]; // Possibly a symbol reference

    let arg1_sym = match &arg1 {
        Node::Symbol(sym) => vm.symbols.borrow().get_symbol(sym.borrow().name()),
        _ => return Err("define expected a symbol as arg 1".to_owned()),
    }; // Definitly a symbol reference

    let val = vm.eval(&args[1])?; // Evalute new value for symbol

    // Mutate symbol in a block so we can use it later
    {
        let mut arg1_sym_mut = arg1_sym.borrow_mut();
        arg1_sym_mut.value = Some(val);
    }

    // Store updated symbol in table
    vm.symbols.borrow_mut().set_local_symbol(arg1_sym.clone());
    Ok(args[0].clone()) // Return symbol
}

fn builtin_setq(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "setq", >=, 2);

    if args.len() % 2 != 0 {
        return Err("setq expected pairs of arguments, received uneven arguments".to_owned());
    }

    for i in 0..(args.len() / 2) {
        let arg1 = args[i * 2];

        let arg1_sym = match &arg1 {
            Node::Symbol(sym) => sym,
            _ => {
                return Err(format!(
                    "setq expected a symbol as first in pair, got {}",
                    arg1.type_str()
                ));
            }
        };

        let val = vm.eval(&args[(i * 2) + 1])?;

        {
            let mut arg1_sym_mut = arg1_sym.borrow_mut();
            arg1_sym_mut.value = Some(val);
        }

        vm.symbols.borrow_mut().set_local_symbol(arg1_sym.clone());
    }

    Ok(Node::Empty)
}

fn builtin_setf(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "setf", 2);

    let arg1 = vm.eval(&args[0])?;

    let arg1_sym = match &arg1 {
        Node::Symbol(sym) => vm.symbols.borrow().get_symbol(sym.borrow().name()),
        _ => {
            return Err(format!(
                "setf expected a symbol as first argument, got {}",
                arg1.type_str()
            ));
        }
    };

    if let Node::Function(f) = vm.eval(&args[1])? {
        let mut arg1_sym_mut = arg1_sym.borrow_mut();
        arg1_sym_mut.function = Some(f);
    }

    vm.symbols.borrow_mut().set_local_symbol(arg1_sym.clone());
    Ok(Node::Empty)
}

fn builtin_defmacro(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "defmacro", 3);

    let macro_name = args[0]; // Possibly a symbol reference

    let macro_sym = match &macro_name {
        Node::Symbol(sym) => vm.symbols.borrow().get_symbol(sym.borrow().name()),
        _ => return Err("defmacro expected a symbol as arg 1".to_owned()),
    }; // Definitly a symbol reference

    let params = match &args[1] {
        Node::List(l) => l,
        _ => {
            return Err(format!(
                "defmacro expected second argument to be a list, got {}",
                args[0].type_str()
            ));
        }
    };

    let mut param_names = Vec::new();
    for param in params.iter() {
        match param {
            Node::Symbol(s) => param_names.push(s.borrow().name().to_owned()),
            _ => {
                return Err(format!(
                    "defmacro expected third argument to be a list of symbols, got {}",
                    param.type_str()
                ));
            }
        }
    }

    {
        let mut macro_sym_mut = macro_sym.borrow_mut();
        macro_sym_mut.function = Some(Callable::Macro(Function {
            params: param_names,
            body: Box::new(args[2].clone()),
        }));
    }

    vm.symbols.borrow_mut().set_local_symbol(macro_sym.clone());
    Ok(Node::Symbol(macro_sym))
}

fn builtin_lambda(_vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "lambda", 2);

    let params = match &args[0] {
        Node::List(l) => l,
        _ => {
            return Err(format!(
                "lambda expected first argument to be a list, got {} ({})",
                args[0].type_str(),
                args[0]
            ));
        }
    };

    let mut param_names = Vec::new();
    for param in params.iter() {
        match param {
            Node::Symbol(s) => param_names.push(s.borrow().name().to_owned()),
            _ => {
                return Err(format!(
                    "lambda expected second argument to be a list of symbols, got {}",
                    param.type_str()
                ));
            }
        }
    }

    Ok(Node::Function(Callable::Func(Function {
        params: param_names,
        body: Box::new(args[1].clone()),
    })))
}

fn builtin_print(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    for arg in args_list.iter() {
        match vm.eval(arg)? {
            Node::List(l) => print!("{:?} ", l),
            n => print!("{} ", n),
        }
    }
    println!();
    Ok(Node::Empty)
}

fn builtin_eval(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "eval", 1);
    vm.eval(args[0])
}

fn builtin_loop(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "loop", 1);
    loop {
        vm.eval(args[0])?;
    }
}

fn builtin_while(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "while", 2);

    loop {
        let cond = vm.eval(args[0])?;
        if !cond.is_truthy() {
            break;
        }
        vm.eval(args[1])?;
    }

    Ok(Node::Empty)
}

fn builtin_progn(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "progn", >=, 1);
    let mut ret = Node::bool_obj(false);

    for form in args {
        if let Node::List(_) = &form {
            ret = vm.eval(&form)?;
        } else {
            return Err(format!(
                "progn expected arguments to be lists, got {}",
                form.type_str()
            ));
        }
    }

    Ok(ret)
}

fn builtin_debug_print_symbol_table(
    vm: &mut VM,
    args_list: ConsList<Node>,
) -> Result<Node, String> {
    // Collect into a vector to make it easier to work with args
    args_setup!(args_list, "debug-print-symbol-table", ==, 0);
    println!("{:?}", vm.symbols);
    Ok(Node::Empty)
}

fn builtin_make_map(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list);

    if args.len() % 2 != 0 {
        return Err("make-map expected pairs of arguments, received uneven arguments".to_owned());
    }

    let mut map = HashMap::with_capacity(args.len() / 2);

    for i in 0..(args.len() / 2) {
        let key = vm.eval(&args[i * 2])?;
        let val = vm.eval(&args[(i * 2) + 1])?;
        map.insert(format!("{}", key), val);
    }

    Ok(Node::from_hashmap(map))
}

fn builtin_get_key(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "get-key", ==, 2);
    let coll = vm.eval(&args[0])?;

    match coll {
        Node::Map(m) => {
            let key = vm.eval(&args[1])?;

            match m.borrow().get(&format!("{}", key)) {
                Some(n) => Ok(n.clone()),
                None => Ok(Node::Empty),
            }
        }
        Node::Symbol(s) => {
            let real_sym = vm.symbols.borrow().get_symbol(s.borrow().name());
            let real_sym_b = real_sym.borrow();
            let key = vm.eval(&args[1])?;

            match real_sym_b.get_property(&format!("{}", key)) {
                Some(n) => Ok(n.clone()),
                None => Ok(Node::Empty),
            }
        }
        _ => Err("get-map requires a hashmap as the first argument".to_owned()),
    }
}

fn builtin_set_key(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "set-key", ==, 3);
    let coll = vm.eval(&args[0])?;

    match coll {
        Node::Map(m) => {
            let key = vm.eval(&args[1])?;
            let val = vm.eval(&args[2])?;

            let prev = m.borrow_mut().insert(format!("{}", key), val);
            match prev {
                Some(v) => Ok(v),
                None => Ok(Node::Empty),
            }
        }
        Node::Symbol(s) => {
            let real_sym = vm.symbols.borrow().get_symbol(s.borrow().name());
            let mut real_sym_b = real_sym.borrow_mut();
            let key = vm.eval(&args[1])?;
            let val = vm.eval(&args[2])?;

            match real_sym_b.set_property(&format!("{}", key), val) {
                Some(n) => Ok(n),
                None => Ok(Node::Empty),
            }
        }
        _ => Err("set-map requires a hashmap as the first argument".to_owned()),
    }
}

fn builtin_has_key(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "has-key?", ==, 2);
    let coll = vm.eval(&args[0])?;

    match coll {
        Node::Map(m) => {
            let key = vm.eval(&args[1])?;

            Ok(Node::bool_obj(m.borrow().contains_key(&format!("{}", key))))
        }
        Node::Symbol(s) => {
            let real_sym = vm.symbols.borrow().get_symbol(s.borrow().name());
            let real_sym_b = real_sym.borrow();
            let key = vm.eval(&args[1])?;

            Ok(Node::bool_obj(real_sym_b.has_property(&format!("{}", key))))
        }
        _ => Err("contains-map requires a hashmap as the first argument".to_owned()),
    }
}

fn builtin_expand_macro(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "expand-macro", ==, 1);

    let list_arg = match args[0] {
        Node::List(l) => l,
        _ => {
            return Err(format!(
                "expand-macro expected argument to be a list, got {}",
                args[0].type_str()
            ));
        }
    };

    let h = list_arg.head();
    if h.is_none() {
        return Ok(Node::Empty);
    }

    let head = h.unwrap();
    if let Node::Symbol(sym_ref) = &head {
        let sym_table_ref = vm.symbols.borrow().get_symbol(sym_ref.borrow().name());
        let sym = sym_table_ref.borrow();
        if let Some(func) = &sym.function {
            if let Callable::Macro(m) = func {
                let macro_args = list_arg.tail();
                let mut new_env = env::Env::with_parent(vm.symbols.clone());

                let args = match m.params.last() {
                    Some(s) => {
                        if s == "&rest" {
                            let mut args = args_setup!(macro_args, "<func1>", >=, m.params.len()-1);
                            let rest = args.split_off(m.params.len() - 1);
                            let rest_list = Node::from_vec_ref(rest);

                            let mut sym = Symbol::new("rest");
                            sym.value = Some(rest_list);
                            new_env.set_symbol(sym.into_ref());

                            args
                        } else {
                            args_setup!(macro_args, "<func2>", m.params.len())
                        }
                    }
                    None => args_setup!(macro_args, "<func>", m.params.len()),
                };

                for (i, arg) in args.iter().enumerate() {
                    let mut sym = Symbol::new(&m.params[i]);
                    sym.value = Some((*arg).clone());
                    new_env.set_symbol(sym.into_ref());
                }

                vm.with_env(new_env.into_ref()).eval(&m.body)
            } else {
                Err(format!("Undefined macro {}", sym.name()))
            }
        } else {
            Err(format!("Undefined macro {}", sym.name()))
        }
    } else {
        Ok(Node::Empty)
    }
}

fn builtin_parse_int(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "parse-int", ==, 1);
    let input = vm.eval(&args[0])?;

    match input {
        Node::String(s) => {
            if let Ok(i) = s.parse::<i64>() {
                Ok(Node::Number(i))
            } else {
                Ok(Node::Empty)
            }
        }
        _ => Err(format!(
            "parse-int can only parse strings, {} given",
            input.type_str()
        )),
    }
}

fn builtin_parse_float(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "parse-float", ==, 1);
    let input = vm.eval(&args[0])?;

    match input {
        Node::String(s) => {
            if let Ok(i) = s.parse::<f64>() {
                Ok(Node::Float(i))
            } else {
                Ok(Node::Empty)
            }
        }
        _ => Err(format!(
            "parse-float can only parse strings, {} given",
            input.type_str()
        )),
    }
}

fn builtin_current_file(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    args_setup!(args_list, "current-file", ==, 0);
    let path = match vm.current_filename() {
        Some(p) => p.to_str().unwrap_or_default(),
        None => "",
    };

    Ok(Node::from_string(path.to_owned()))
}
