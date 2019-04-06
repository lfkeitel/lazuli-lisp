mod env;

use crate::object::cons_list::ConsList;
use crate::object::{self, Node, Program};

#[derive(Default)]
pub struct VM {
    symbols: env::Env,
}

macro_rules! make_builtin {
    ($vm:ident, $sym:expr, $func:ident) => {{
        let mut sym = object::Symbol::new($sym);
        sym.function = Some(object::Callable::Builtin($func));
        $vm.symbols.set_symbol(sym.into_ref());
    }};
}

impl VM {
    pub fn new() -> Self {
        let mut vm = VM {
            symbols: env::Env::new(),
        };

        make_builtin!(vm, "defvar", builtin_defvar);
        make_builtin!(vm, "setq", builtin_setq);
        make_builtin!(vm, "print", builtin_print);
        make_builtin!(vm, "quote", builtin_quote);
        make_builtin!(vm, "quasiquote", builtin_quasiquote);
        make_builtin!(vm, "progn", builtin_progn);
        make_builtin!(
            vm,
            "debug-print-symbol-table",
            builtin_debug_print_symbol_table
        );
        make_builtin!(vm, "+", builtin_add);
        make_builtin!(vm, "-", builtin_sub);
        make_builtin!(vm, "*", builtin_mul);
        make_builtin!(vm, "/", builtin_div);

        vm
    }

    pub fn add_symbol(&mut self, sym: object::SymbolRef) {
        self.symbols.set_symbol(sym);
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
                Ok(self.symbols.get_symbol(&sym_name).borrow().value())
            }
            Node::Number(num) => Ok(Node::Number(*num)),
            Node::String(string) => Ok(Node::String(string.to_owned())),
            Node::List(list) => self.eval_list(&list),
            // Node::Function(Function),
            _ => Err("Not supported".to_owned()),
        }
    }

    fn eval_list(&mut self, form: &ConsList<Node>) -> Result<Node, String> {
        let h = form.head();
        if h.is_none() {
            return Ok(Node::empty_list());
        }

        match &h.unwrap() {
            Node::Symbol(sym_ref) => {
                let sym_table_ref = self.symbols.get_symbol(sym_ref.borrow().name());
                let sym = sym_table_ref.borrow();
                if let Some(func) = &sym.function {
                    match func {
                        object::Callable::Builtin(func) => func(self, form.tail()),
                        object::Callable::User(_) => {
                            Err(format!("User functions not supported yet {}", sym.name()))
                        }
                    }
                } else {
                    Err(format!("Undefined function {}", sym.name()))
                }
            }
            _ => Err("Cannot evaluate non-symbol object".to_owned()),
        }
    }
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
        args_setup!($args_list, $name, =, $check)
    };
}

fn builtin_defvar(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    // Collect into a vector to make it easier to work with args
    let args = args_setup!(args_list, "defavar", ==, 2);

    let arg1 = args[0]; // Possibly a symbol reference

    let arg1_sym = match &arg1 {
        Node::Symbol(sym) => sym,
        _ => return Err("defvar expected a symbol as arg 1".to_owned()),
    }; // Definitly a symbol reference

    let val = vm.eval(&args[1])?; // Evalute new value for symbol

    // Mutate symbol in a block so we can use it later
    {
        let mut arg1_sym_mut = arg1_sym.borrow_mut();
        arg1_sym_mut.value = Some(val);
    }

    // Store updated symbol in table
    vm.symbols.set_symbol(arg1_sym.clone());
    Ok(args[0].clone()) // Return symbol
}

fn builtin_setq(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "defavar", >=, 2);

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

        vm.symbols.set_symbol(arg1_sym.clone());
    }

    Ok(object::Node::empty_list())
}

fn builtin_print(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    if !args_list.empty() {
        for arg in args_list.iter() {
            print!("{} ", vm.eval(arg)?);
        }
        println!("");
    }
    Ok(object::Node::empty_list())
}

fn builtin_quote(_vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "quote", ==, 1);
    Ok(args[0].clone())
}

fn builtin_quasiquote(_vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    // TODO: Expand internal unquotes within a list
    let args = args_setup!(args_list, "quote", ==, 1);
    Ok(args[0].clone())
}

fn builtin_progn(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "progn", >=, 1);
    let mut ret = object::Node::empty_list();

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
    Ok(object::Node::empty_list())
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
