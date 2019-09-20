use crate::args_setup;
use crate::object::cons_list::ConsList;
use crate::object::Node;
use crate::vm::VM;

pub(crate) fn logic_gt(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, ">", ==, 2);
    let arg1 = vm.eval(args[0])?;
    let arg2 = vm.eval(args[1])?;

    Ok(Node::bool_obj(arg1 > arg2))
}

pub(crate) fn logic_lt(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "<", ==, 2);
    let arg1 = vm.eval(args[0])?;
    let arg2 = vm.eval(args[1])?;

    Ok(Node::bool_obj(arg1 < arg2))
}

pub(crate) fn logic_eq(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "eq", >=, 2);
    let evaled_arg1 = vm.eval(args[0]).unwrap_or_default();

    let res = args
        .iter()
        .skip(1)
        .all(|x| vm.eval(&x).unwrap_or_default() == evaled_arg1);

    Ok(Node::bool_obj(res))
}

pub(crate) fn logic_not(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "not", ==, 1);
    Ok(Node::bool_obj(!vm.eval(args[0])?.is_truthy()))
}

pub(crate) fn logic_and(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "and", >=, 2);
    let res = args
        .iter()
        .all(|x| vm.eval(&x).unwrap_or_default().is_truthy());
    Ok(Node::bool_obj(res))
}

pub(crate) fn logic_or(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "or", >=, 2);
    let res = args
        .iter()
        .any(|x| vm.eval(&x).unwrap_or_default().is_truthy());
    Ok(Node::bool_obj(res))
}

pub(crate) fn logic_if(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "if", 2, 3);
    let check = vm.eval(&args[0])?;

    if check.is_truthy() {
        vm.eval(&args[1])
    } else if args.len() == 3 {
        vm.eval(&args[2])
    } else {
        Ok(Node::Empty)
    }
}
