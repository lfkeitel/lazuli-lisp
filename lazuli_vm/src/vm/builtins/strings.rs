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

pub(crate) fn string_replace(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "string-replace", >=, 3);

    if args.len() > 4 {
        return Err("string-replace requires 3-4 args".to_owned());
    }

    let src = match vm.eval(&args[0])? {
        Node::String(s) => s,
        _ => return Err("string-replace arg 1 must be a string".to_owned()),
    };

    let old = match vm.eval(&args[1])? {
        Node::String(s) => s,
        _ => return Err("string-replace arg 2 must be a string".to_owned()),
    };

    let new = match vm.eval(&args[2])? {
        Node::String(s) => s,
        _ => return Err("string-replace arg 3 must be a string".to_owned()),
    };

    if args.len() == 3 {
        Ok(Node::from_string(src.replace(&old, &new)))
    } else {
        let count = match vm.eval(&args[3])? {
            Node::Number(n) => n,
            _ => return Err("string-replace arg 4 must be a number".to_owned()),
        };

        Ok(Node::from_string(src.replacen(&old, &new, count as usize)))
    }
}

pub(crate) fn string_split(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "string-split", >=, 2);

    if args.len() > 3 {
        return Err("string-split requires 2-3 args".to_owned());
    }

    let src = match vm.eval(&args[0])? {
        Node::String(s) => s,
        _ => return Err("string-split arg 1 must be a string".to_owned()),
    };

    let split = match vm.eval(&args[1])? {
        Node::String(s) => s,
        _ => return Err("string-split arg 2 must be a string".to_owned()),
    };

    if args.len() == 2 {
        let splits = src
            .split(&split)
            .map(|x| Node::from_string(x.to_owned()))
            .collect();
        Ok(Node::from_vec(splits))
    } else {
        let count = match vm.eval(&args[2])? {
            Node::Number(n) => n,
            _ => return Err("string-replace arg 4 must be a number".to_owned()),
        };

        let splits = src
            .splitn(count as usize, &split)
            .map(|x| Node::from_string(x.to_owned()))
            .collect();
        Ok(Node::from_vec(splits))
    }
}
