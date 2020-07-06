use super::ast::*;
use super::errors::{Error, Result};
use super::parsers;
use super::streams;

pub fn program(program: parsers::Program) -> Result<Program> {
    let source = self::source(program.pipe.source.value)?;
    let sink = self::sink(program.pipe.sink.value)?;
    Ok(Program {
        pipe: Pipe { source, sink },
    })
}

fn source(expression: parsers::Expression) -> Result<Box<dyn Source>> {
    match expression {
        parsers::Expression::Identifier(id) if id == "stdin" => Ok(Box::new(streams::Stdin::new())),
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}

fn sink(expression: parsers::Expression) -> Result<Box<dyn Sink>> {
    match expression {
        parsers::Expression::Identifier(id) if id == "stdout" => {
            Ok(Box::new(streams::Stdout::new()))
        }
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}
