use crate::object::cons_list::ConsList;
use crate::object::{self, Node};
use crate::vm::VM;
use crate::args_setup;

pub(crate) fn quote(_vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "quote", 1);
    Ok(args[0].clone())
}

pub(crate) fn quasiquote(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    // TODO: Expand internal unquotes within a list
    let args = args_setup!(args_list, "quasiquote", 1);
    let arg = match args[0] {
        Node::List(l) => l,
        _ => unreachable!(),
    };

    let mut unquoted = Vec::with_capacity(arg.len());

    for item in arg.iter() {
        match item {
            Node::List(l) => {
                if l.is_empty() {
                    unquoted.push(item.clone());
                    continue;
                }

                if let Node::Symbol(s) = l.head().unwrap() {
                    match s.borrow().name() {
                        "unquote" => {
                            unquoted.push(vm.eval(l.tail().head().unwrap())?);
                        }
                        "unquote-splice" => {
                            let evaled = vm.eval(l.tail().head().unwrap())?;
                            if let Node::List(l) = evaled {
                                for e in l.iter() {
                                    unquoted.push(e.clone());
                                }
                            } else {
                                return Err(format!(
                                    "unquote-splice must return a list, got {}",
                                    evaled.type_str()
                                ));
                            }
                        }
                        _ => unquoted.push(quasiquote(vm, ConsList::new().append(item.clone()))?), // Recursively unquote inner lists
                    }
                } else {
                    unquoted.push(item.clone());
                }
            }
            _ => unquoted.push(item.clone()), // Only lists get special treatment
        }
    }

    Ok(object::Node::List(
        unquoted
            .into_iter()
            .rev()
            .fold(object::cons_list::ConsList::new(), |acc, elem| {
                acc.append(elem)
            }),
    ))
}
