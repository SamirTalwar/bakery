use super::ast::Expression;
use super::errors;
use super::errors::{Error, Result};

pub fn interpret(expression: Expression) -> Result<()> {
    let mut buffer: Vec<u8> = Vec::new();
    match expression {
        Expression::Pipe { source, sink } => match (*source, *sink) {
            (Expression::Block(source_block), Expression::Block(sink_block)) => {
                let mut open_source = source_block.source()?;
                open_source.read_to_end(&mut buffer).map_err(errors::io)?;
                let mut open_sink = sink_block.sink()?;
                open_sink.write_all(&buffer[..]).map_err(errors::io)?;
                Ok(())
            }
            _ => Err(Error::UninterpretableProgram),
        },
        _ => Err(Error::UninterpretableProgram),
    }
}
