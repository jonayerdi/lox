use std::{
    env,
    fs::File,
    io::{self, Read},
};

use lox::{
    context::Position,
    interpreter::Interpreter,
    result::{LoxError, Result},
};

fn run() -> Result<(), Position> {
    let args = env::args().skip(1).collect::<Vec<_>>();
    match args.len() {
        0 => lox::run_interactive(
            &mut Interpreter::new(),
            io::stdin().lock(),
            io::stdout().lock(),
        ),
        1 => {
            let filename = &args[0];
            let mut source = String::new();
            File::open(filename)?.read_to_string(&mut source)?;
            lox::run(&mut Interpreter::new(), &source, io::stdout().lock())
        }
        _ => Err(LoxError::other("Usage: loxi [script]")),
    }
}

fn main() {
    if let Err(error) = run() {
        eprintln!("{error}");
    }
}
