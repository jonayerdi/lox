use std::io::{BufRead, Write};

use crate::result::Result;

pub fn run_interactive<R: BufRead, W: Write>(mut input: R, mut output: W) -> Result<()> {
    let mut buf = String::with_capacity(80);
    loop {
        write!(output, "> ")?;
        output.flush()?;
        if input.read_line(&mut buf)? == 0 {
            break; // EOF reached
        }
        run(&buf, &mut output)?;
    }
    Ok(())
}

pub fn run<W: Write>(_source: &str, _output: W) -> Result<()> {
    todo!()
}
