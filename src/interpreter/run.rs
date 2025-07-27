use std::io::{BufRead, Write};

use crate::{interpreter::Interpreter, parser::Parser, result::Result, scanner::Scanner};

pub fn run_interactive(interpreter: &mut Interpreter) -> Result<()> {
    let mut buf = String::with_capacity(80);
    loop {
        write!(interpreter.stdout, "> ")?;
        interpreter.stdout.flush()?;
        if interpreter.stdin.read_line(&mut buf)? == 0 {
            break; // EOF reached
        }
        if let Err(error) = run(interpreter, &buf) {
            writeln!(interpreter.stderr, "{}", error)?;
        }
        buf.clear();
    }
    Ok(())
}

pub fn run(interpreter: &mut Interpreter, source: &str) -> Result<()> {
    let scanner = Scanner::new(source.chars());
    let tokens = scanner.collect::<Result<Vec<_>>>()?;
    let mut parser = Parser::new(tokens.into_iter());
    let statements = parser.parse()?;
    interpreter.interpret(&statements)?;
    Ok(())
}
