use std::{env, fs::File, io::Read};

use lox::{
    interpreter::Interpreter,
    result::{LoxError, Result},
};

fn run() -> Result<()> {
    let args = env::args().skip(1).collect::<Vec<_>>();
    match args.len() {
        0 => lox::run_interactive(&mut Interpreter::new()),
        1 => {
            let filename = &args[0];
            let mut source = String::new();
            File::open(filename)?.read_to_string(&mut source)?;
            lox::run(&mut Interpreter::new(), &source)
        }
        _ => Err(LoxError::cli("Usage: loxi [script]")),
    }
}

fn main() {
    if let Err(error) = run() {
        eprintln!("{error}");
    }
}
