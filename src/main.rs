#![forbid(unsafe_code)]

mod lox;
mod parser;
mod position;
mod result;
mod rewind;
mod scanner;
mod token;

use std::{
    env,
    fs::File,
    io::{self, Read},
};

use result::{LoxError, Result};

fn run() -> Result<()> {
    let args = env::args().skip(1).collect::<Vec<_>>();
    match args.len() {
        0 => lox::run_interactive(io::stdin().lock(), io::stdout().lock()),
        1 => {
            let filename = &args[0];
            let mut source = String::new();
            File::open(filename)?.read_to_string(&mut source)?;
            lox::run(&source, io::stdout().lock())
        }
        _ => Err(LoxError::other("Usage: lox [script]")),
    }
}

fn main() {
    if let Err(error) = run() {
        eprintln!("{error}");
    }
}
