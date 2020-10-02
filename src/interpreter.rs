use std::io;

use super::ast::{Block, Expression};
use super::errors;
use super::errors::{Error, Result};

pub fn interpret(expression: Expression) -> Result<()> {
    match expression {
        Expression::Pipe { source, sink } => {
            let mut open_source = source.source().map(io::BufReader::new)?;
            let mut open_sink = sink.sink().map(io::BufWriter::new)?;
            io::copy(&mut open_source, &mut open_sink).map_err(errors::io)?;
            Ok(())
        }
        _ => Err(Error::UninterpretableProgram),
    }
}
