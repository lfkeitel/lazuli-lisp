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

