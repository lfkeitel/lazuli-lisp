use crate::object::cons_list::ConsList;
use crate::object::{self, Node};
use crate::vm::VM;
use crate::args_setup;

macro_rules! arithmetic_fn {
    ($fnname:ident, $oper:tt, $sym:expr) => {
        pub(crate) fn $fnname(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
            // Collect into a vector to make it easier to work with args
            let args = args_setup!(args_list, $sym, >=, 2);

            match vm.eval(args[0])? {
                Node::Number(n) => {
                    let mut val = n;

                    for arg in &args[1..] {
                        let evaled_arg = vm.eval(arg)?;

                        match evaled_arg {
                            Node::Number(n) => val $oper n,
                            Node::Float(n) => val $oper n as i64,
                            n => return Err(format!("{} expected number arguments, got {}", $sym, n.type_str())),
                        }
                    }

                    Ok(object::Node::Number(val))
                }

                Node::Float(n) => {
                    let mut val = n;

                    for arg in &args[1..] {
                        let evaled_arg = vm.eval(arg)?;

                        match evaled_arg {
                            Node::Number(n) => val $oper n as f64,
                            Node::Float(n) => val $oper n,
                            n => return Err(format!("{} expected float arguments, got {}", $sym, n.type_str())),
                        }
                    }

                    Ok(object::Node::Float(val))
                }

                node => Err(format!("{} expected int or float arguments, got {}", $sym, node.type_str()))
            }
        }
    };
}

arithmetic_fn!(add, +=, "+");
arithmetic_fn!(sub, -=, "-");
arithmetic_fn!(mul, *=, "*");
arithmetic_fn!(div, /=, "/");
