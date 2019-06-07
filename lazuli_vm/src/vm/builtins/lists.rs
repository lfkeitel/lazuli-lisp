use crate::args_setup;
use crate::object::cons_list::ConsList;
use crate::object::Node;
use crate::vm::VM;


pub(crate) fn make_list(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list);
    let mut new_list = ConsList::new();
    for item in args.iter().rev() {
        new_list = new_list.append(vm.eval(item)?);
    }
    Ok(Node::List(new_list))
}

pub(crate) fn concat(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list);
    let mut new_list = ConsList::new();
    for item in args.iter().rev() {
        match vm.eval(item)? {
            Node::List(l) => {
                let list_items = l.iter().collect::<Vec<&Node>>();
                for i in list_items.iter().rev() {
                    new_list = new_list.append((*i).clone());
                }
            }
            _ => return Err(format!("concat expected a list, got a {}", item.type_str())),
        }
    }
    Ok(Node::List(new_list))
}

pub(crate) fn head(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "head", 1);
    match vm.eval(args[0])? {
        Node::List(l) => Ok(match l.head() {
            Some(n) => n.clone(),
            None => Node::empty_list(),
        }),
        _ => Err(format!(
            "head expected a list, got a {}",
            args[0].type_str()
        )),
    }
}

pub(crate) fn tail(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "tail", 1);
    match vm.eval(args[0])? {
        Node::List(l) => Ok(Node::List(l.tail())),
        _ => Err(format!(
            "tail expected a list, got a {}",
            args[0].type_str()
        )),
    }
}
