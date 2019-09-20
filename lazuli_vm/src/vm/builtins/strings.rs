use crate::args_setup;
use crate::object::cons_list::ConsList;
use crate::object::Node;
use crate::vm::VM;

pub(crate) fn string_concat(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list);
    let mut new_str = String::new();

    for (i, item) in args.iter().enumerate() {
        match vm.eval(item)? {
            Node::String(s) => {
                new_str.push_str(&s);
            }
            _ => {
                return Err(format!(
                    "string-concat expected a string, got a {} for arg {}",
                    item.type_str(),
                    i
                ))
            }
        }
    }

    Ok(Node::from_string(new_str))
}
