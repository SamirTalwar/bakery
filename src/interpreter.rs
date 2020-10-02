use super::ast::{Block, Expression};
use super::errors::{Error, Result};

pub fn interpret(expression: Expression) -> Result<()> {
    match expression {
        Expression::Pipe { source, sink } => {
            let open_sink = sink.sink()?;
            source.source(open_sink)
        }
        _ => Err(Error::UninterpretableProgram),
    }
}
