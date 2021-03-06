extern crate clap;

use std::path::Path;

use lazuli_vm::{compiler, vm};

use clap::{App, Arg};
use path_absolutize::*;

fn main() {
    let app = App::new("Lazuli Lisp")
        .version("0.1.0")
        .author("Lee Keitel")
        .about("Execute a lisp file")
        .arg(Arg::with_name("INPUT").required(true))
        .get_matches();

    compile_file(app.value_of("INPUT").unwrap());
}

fn compile_file(path: &str) {
    let src_path = Path::new(path);
    let code = compiler::compile_file(src_path).unwrap_or_else(|e| {
        eprintln!("{}", e);
        std::process::exit(1);
    });

    let mut vm = vm::VM::new();
    vm.add_filename(src_path.absolutize().unwrap_or_default());
    if let Err(e) = vm.run(&code) {
        println!("Error: {}", e);
    }
}
