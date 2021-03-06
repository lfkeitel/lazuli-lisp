use std::env;
use std::path::Path;

use crate::args_setup;
use crate::compiler;
use crate::compiler::parser::ParserError;
use crate::object::cons_list::ConsList;
use crate::object::Node;
use crate::vm::VM;

pub(crate) fn include(vm: &mut VM, args_list: ConsList<Node>) -> Result<Node, String> {
    let args = args_setup!(args_list, "include", 1);

    let new_path_str = match vm.eval(args[0])? {
        Node::String(s) => s,
        Node::Symbol(sym) => match sym.borrow().value() {
            Node::String(s) => s,
            _ => return Err("include expected a string".to_owned()),
        },
        _ => return Err("include expected a string".to_owned()),
    };

    let import_path = Path::new(&new_path_str);

    let full_import_path = if import_path.is_absolute() {
        import_path.to_path_buf()
    } else {
        let mut rel_dir = match vm.current_filename() {
            Some(s) => match Path::new(s).parent() {
                Some(p) => p.to_owned(),
                None => return Err("Can't determine script directory".to_owned()),
            },
            None => match env::current_dir() {
                Ok(s) => s.as_path().to_owned(),
                Err(_) => return Err("Can't determine script directory".to_owned()),
            },
        };

        rel_dir.push(import_path);
        rel_dir
    };

    match compiler::compile_file(full_import_path.as_path()) {
        Ok(code) => {
            vm.add_filename(full_import_path);
            let res = vm.run(&code);
            vm.pop_filename();
            res
        }
        Err(e) => match e {
            ParserError::FileNotFound(_) => Err(format!(
                "Error including file: File not found {}",
                full_import_path.to_string_lossy()
            )),
            _ => Err(format!("Error including file: {}", e)),
        },
    }
}
