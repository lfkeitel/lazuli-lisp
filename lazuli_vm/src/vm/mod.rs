mod env;

use crate::object::cons_list::ConsList;
use crate::object::{self, Callable, Function, Node, Program, Symbol};

#[derive(Default)]
pub struct VM {
    symbols: env::EnvRef,
}

macro_rules! make_builtin {
    ($vm:ident, $sym:expr, $func:ident) => {{
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

    ($args_list:ident, $sym:expr, $oper:tt, $check:expr) => {
        {
            let args: Vec<&Node> = $args_list.iter().collect();

            if !(args.len() $oper $check) {
                return Err(format!(
                    args_setup_error!($oper),
                    $sym,
                    $check,
                    args.len()
                ))
            }

            args
        }
    };

    ($args_list:ident, $name:expr, $check:expr) => {
        args_setup!($args_list, $name, ==, $check)
    };
}

impl VM {
    pub fn new() -> Self {
        let vm = VM {
            symbols: env::Env::new().into_ref(),
        };

        make_builtin!(vm, "defvar", builtin_defvar);
        make_builtin!(vm, "defmacro", builtin_defmacro);
        make_builtin!(vm, "setq", builtin_setq);
        make_builtin!(vm, "setf", builtin_setf);
        make_builtin!(vm, "print", builtin_print);
        make_builtin!(vm, "quote", builtin_quote);
        make_builtin!(vm, "quasiquote", builtin_quasiquote);
        make_builtin!(vm, "progn", builtin_progn);
        make_builtin!(vm, "lambda", builtin_lambda);
        make_builtin!(vm, "list", builtin_list);
        make_builtin!(vm, "eval", builtin_eval);
        make_builtin!(
            vm,
            "debug-print-symbol-table",
            builtin_debug_print_symbol_table
        );
        make_builtin!(vm, "+", builtin_add);
        make_builtin!(vm, "-", builtin_sub);
        make_builtin!(vm, "*", builtin_mul);
        make_builtin!(vm, "/", builtin_div);
        make_builtin!(vm, "eq", builtin_eq);
        make_builtin!(vm, "not", builtin_not);
        make_builtin!(vm, "and", builtin_and);
        make_builtin!(vm, "or", builtin_or);

        vm
    }

    pub fn with_env(env: env::EnvRef) -> Self {
        VM { symbols: env }
    }

    pub fn add_symbol(&mut self, sym: object::SymbolRef) {
        self.symbols.borrow_mut().set_symbol(sym);
    }

    pub fn run(&mut self, program: &Program) -> Result<(), String> {
        // println!("{:?}", self.symbols);
        for form in program.iter() {
            self.eval(&form).map(|_| ())?
        }
        Ok(())
    }

    pub fn eval(&mut self, form: &Node) -> Result<Node, String> {
        match form {
            Node::Symbol(sym_ref) => {
                let sym = sym_ref.borrow();
                let sym_name = sym.name();
                Ok(self.symbols.borrow().get_symbol(&sym_name).borrow().value())
            }
            Node::Number(num) => Ok(Node::Number(*num)),
            Node::String(string) => Ok(Node::String(string.to_owned())),
            Node::List(list) => self.eval_list(&list),
            Node::Keyword(v) => Ok(Node::Keyword(v.clone())),
            _ => Err("Not supported".to_owned()),
        }
    }

    fn eval_function(&mut self, func: &Callable, args: ConsList<Node>) -> Result<Node, String> {
        match func {
            Callable::Builtin(f) => f(self, args),
            Callable::Func(f) => {
                let args = args_setup!(args, "<func>", f.params.len());
                let mut new_env = env::Env::with_parent(self.symbols.clone());

                for (i, arg) in args.iter().enumerate() {
                    let mut sym = Symbol::new(&f.params[i]);
                    sym.value = Some(self.eval(arg)?);
                    new_env.set_symbol(sym.into_ref());
                }

                VM::with_env(new_env.into_ref()).eval(&f.body)
            }
            Callable::Macro(f) => {
                let args = args_setup!(args, "<func>", f.params.len());
                let mut new_env = env::Env::with_parent(self.symbols.clone());

                for (i, arg) in args.iter().enumerate() {
                    let mut sym = Symbol::new(&f.params[i]);
                    sym.value = Some((*arg).clone());
                    new_env.set_symbol(sym.into_ref());
                }

                let expanded = VM::with_env(new_env.into_ref()).eval(&f.body)?;
                self.eval(&expanded)
            }
        }
    }

    fn eval_list(&mut self, form: &ConsList<Node>) -> Result<Node, String> {
        let h = form.head();
        if h.is_none() {
            return Ok(Node::bool_obj(false));
        }

        let head = h.unwrap();
        if let Node::Symbol(sym_ref) = &head {
            let sym_table_ref = self.symbols.borrow().get_symbol(sym_ref.borrow().name());
            let sym = sym_table_ref.borrow();
            return if let Some(func) = &sym.function {
                self.eval_function(&func, form.tail())
            } else {
                Err(format!("Undefined function {}", sym.name()))
            };
        }

        let evaled_head = self.eval(&head)?;

        match &evaled_head {
            Node::Symbol(sym_ref) => {
                let sym_table_ref = self.symbols.borrow().get_symbol(sym_ref.borrow().name());
                let sym = sym_table_ref.borrow();
                if let Some(func) = &sym.function {
                    self.eval_function(&func, form.tail())
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

fn builtin_defvar(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    // Collect into a vector to make it easier to work with args
    let args = args_setup!(args_list, "defavar", 2);

    let arg1 = args[0]; // Possibly a symbol reference

    let arg1_sym = match &arg1 {
        Node::Symbol(sym) => vm.symbols.borrow().get_symbol(sym.borrow().name()),
        _ => return Err("defvar expected a symbol as arg 1".to_owned()),
    }; // Definitly a symbol reference

    let val = vm.eval(&args[1])?; // Evalute new value for symbol

    // Mutate symbol in a block so we can use it later
    {
        let mut arg1_sym_mut = arg1_sym.borrow_mut();
        arg1_sym_mut.value = Some(val);
    }

    // Store updated symbol in table
    vm.symbols.borrow_mut().set_symbol(arg1_sym.clone());
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

        vm.symbols.borrow_mut().set_symbol(arg1_sym.clone());
    }

    Ok(Node::bool_obj(false))
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

    vm.symbols.borrow_mut().set_symbol(arg1_sym.clone());
    Ok(Node::bool_obj(false))
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

    vm.symbols.borrow_mut().set_symbol(macro_sym.clone());
    Ok(Node::Symbol(macro_sym))
}

fn builtin_lambda(_vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "lambda", 2);

    let params = match &args[0] {
        Node::List(l) => l,
        _ => {
            return Err(format!(
                "lambda expected first argument to be a list, got {}",
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
    if !args_list.empty() {
        for arg in args_list.iter() {
            print!("{} ", vm.eval(arg)?);
        }
        println!("");
    }
    Ok(Node::bool_obj(false))
}

fn builtin_list(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list);
    let mut new_list = ConsList::new();
    for item in args.iter().rev() {
        new_list = new_list.append(vm.eval(item)?);
    }
    Ok(Node::List(new_list))
}

fn builtin_quote(_vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "quote", 1);
    Ok(args[0].clone())
}

fn builtin_quasiquote(_vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    // TODO: Expand internal unquotes within a list
    let args = args_setup!(args_list, "quasiquote", 1);
    Ok(args[0].clone())
}

fn builtin_eval(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "eval", 1);
    vm.eval(args[0])
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
    Ok(Node::bool_obj(false))
}

macro_rules! arithmetic_fn {
    ($fnname:ident, $oper:tt, $sym:expr) => {
        fn $fnname(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
            // Collect into a vector to make it easier to work with args
            let args = args_setup!(args_list, $sym, >=, 2);

            // Get starting value
            let mut val = {
                let evaled_arg = vm.eval(args[0])?;
                match evaled_arg {
                    Node::Number(n) => n,
                    n => return Err(format!("{} expected number arguments, got {}", $sym, n.type_str())),
                }
            };

            for arg in &args[1..] {
                let evaled_arg = vm.eval(arg)?;

                match evaled_arg {
                    Node::Number(n) => val = val $oper n,
                    n => return Err(format!("{} expected number arguments, got {}", $sym, n.type_str())),
                }
            }

            Ok(object::Node::Number(val))
        }
    };
}

arithmetic_fn!(builtin_add, +, "+");
arithmetic_fn!(builtin_sub, -, "-");
arithmetic_fn!(builtin_mul, *, "*");
arithmetic_fn!(builtin_div, /, "/");

fn builtin_eq(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "eq", >=, 2);
    let evaled_arg1 = vm.eval(args[0]).unwrap_or_default();

    let res = args
        .iter()
        .skip(1)
        .all(|x| vm.eval(&x).unwrap_or_default() == evaled_arg1);

    Ok(Node::bool_obj(res))
}

fn builtin_not(_: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "not", ==, 1);
    Ok(Node::bool_obj(!args[0].is_truthy()))
}

fn builtin_and(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "and", >=, 2);
    let res = args
        .iter()
        .all(|x| vm.eval(&x).unwrap_or_default().is_truthy());
    Ok(Node::bool_obj(res))
}

fn builtin_or(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "or", >=, 2);
    let res = args
        .iter()
        .any(|x| vm.eval(&x).unwrap_or_default().is_truthy());
    Ok(Node::bool_obj(res))
}
