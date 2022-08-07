use std::io::{BufRead, Write};

use crate::{interpreter::Interpreter, parser::Parser, result::Result, scanner::Scanner};

pub fn run_interactive<R: BufRead, W: Write>(
    interpreter: &mut Interpreter,
    mut input: R,
    mut output: W,
) -> Result<()> {
    let mut buf = String::with_capacity(80);
    loop {
        write!(output, "> ")?;
        output.flush()?;
        if input.read_line(&mut buf)? == 0 {
            break; // EOF reached
        }
        run(interpreter, &buf, &mut output)?;
        buf.clear();
    }
    Ok(())
}

pub fn run<W: Write>(interpreter: &mut Interpreter, source: &str, _output: W) -> Result<()> {
    let scanner = Scanner::new(source.chars());
    let tokens = scanner.collect::<Result<Vec<_>>>()?;
    let mut parser = Parser::new(tokens.iter());
    let expr = parser.parse()?;
    let result = interpreter.evaluate_expression(&expr)?;
    println!("{}", result);
    Ok(())
}
