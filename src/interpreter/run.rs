use std::io::{BufRead, Write};

use crate::{context::Position, interpreter::Interpreter, parser::Parser, result::Result, scanner};

pub fn run_interactive<R: BufRead, W: Write>(
    interpreter: &mut Interpreter,
    mut input: R,
    mut output: W,
) -> Result<(), Position> {
    let mut buf = String::with_capacity(80);
    loop {
        write!(output, "> ")?;
        output.flush()?;
        if input.read_line(&mut buf)? == 0 {
            break; // EOF reached
        }
        if let Err(error) = run(interpreter, &buf, &mut output) {
            writeln!(output, "{}", error)?;
        }
        buf.clear();
    }
    Ok(())
}

pub fn run<W: Write>(
    interpreter: &mut Interpreter,
    source: &str,
    mut output: W,
) -> Result<(), Position> {
    let scanner = scanner::with_positions(source.chars());
    let tokens = scanner.collect::<Result<Vec<_>, _>>()?;
    let mut parser = Parser::new(tokens.into_iter());
    let expr = parser.parse()?;
    let result = interpreter.evaluate_expression(&expr)?;
    writeln!(output, "{}", result)?;
    Ok(())
}
