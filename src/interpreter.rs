use std::io;

use super::ast::Expression;
use super::errors;
use super::errors::{Error, Result};

pub fn interpret(expression: Expression) -> Result<()> {
    match expression {
        Expression::Pipe { source, sink } => match (*source, *sink) {
            (Expression::Block(source_block), Expression::Block(sink_block)) => {
                let mut open_source = source_block.source().map(io::BufReader::new)?;
                let mut open_sink = sink_block.sink().map(io::BufWriter::new)?;
                io::copy(&mut open_source, &mut open_sink).map_err(errors::io)?;
                Ok(())
            }
            _ => Err(Error::UninterpretableProgram),
        },
        _ => Err(Error::UninterpretableProgram),
    }
}
