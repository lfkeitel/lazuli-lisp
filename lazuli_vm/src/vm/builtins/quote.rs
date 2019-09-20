use crate::args_setup;
use crate::object::cons_list::ConsList;
use crate::object::{self, Node};
use crate::vm::VM;

pub(crate) fn quote(_vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "quote", 1);
    Ok(args[0].clone())
}

pub(crate) fn quasiquote(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "quasiquote", 1);
    let arg = match args[0] {
        Node::List(l) => l,
        _ => unreachable!(),
    };

    let mut unquoted = Vec::with_capacity(arg.len());

    for item in arg.iter() {
        unquote_item(vm, item, &mut unquoted)?
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

fn unquote_item(vm: &mut VM, item: &Node, unquoted: &mut Vec<Node>) -> Result<(), String> {
    match item {
        Node::List(outer_list) => {
            if outer_list.is_empty() {
                unquoted.push(item.clone());
                return Ok(());
            }

            let inner = outer_list.head().unwrap();
            match inner {
                Node::Symbol(s) => {
                    match s.borrow().name() {
                        "stop-unquote" => {
                            unquoted.push(outer_list.tail().head().unwrap().clone());
                        }
                        "unquote" => {
                            unquoted.push(vm.eval(outer_list.tail().head().unwrap())?);
                        }
                        "unquote-splice" => {
                            let evaled = vm.eval(outer_list.tail().head().unwrap())?;
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
                }
                Node::List(_) => {
                    let mut inner_unquoted = Vec::new();
                    for l_inner in outer_list.iter() {
                        unquote_item(vm, l_inner, &mut inner_unquoted)?;
                    }
                    unquoted.push(Node::from_vec(inner_unquoted));
                }
                _ => unquoted.push(item.clone()),
            }
        }
        _ => unquoted.push(item.clone()), // Only lists get special treatment
    }
    Ok(())
}
