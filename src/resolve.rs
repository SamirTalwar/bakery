use super::ast::*;
use super::errors::{Error, Result};
use super::parsers;
use super::streams;

pub fn program(program: parsers::Program) -> Result<Program> {
    match *program.expression.value {
        parsers::Expression::Text { contents } => {
            let source = Box::new(streams::Text::new(contents));
            let sink = Box::new(streams::Stdout::new());
            Ok(Program {
                pipe: Pipe { source, sink },
            })
        }
        expression
        @
        parsers::Expression::Identifier {
            namespace: _,
            id: _,
        } => {
            let source = self::source(expression)?;
            let sink = Box::new(streams::Stdout::new());
            Ok(Program {
                pipe: Pipe { source, sink },
            })
        }
        parsers::Expression::Pipe { source, sink } => {
            let source = self::source(*source.value)?;
            let sink = self::sink(*sink.value)?;
            Ok(Program {
                pipe: Pipe { source, sink },
            })
        }
    }
}

fn source(expression: parsers::Expression) -> Result<Box<dyn Source>> {
    match expression {
        parsers::Expression::Text { contents } => Ok(Box::new(streams::Text::new(contents))),
        parsers::Expression::Identifier { namespace: _, id } if id == "stdin" => {
            Ok(Box::new(streams::Stdin::new()))
        }
        parsers::Expression::Identifier { namespace, id } if namespace == "file" => {
            Ok(Box::new(streams::File::new(id)))
        }
        parsers::Expression::Identifier { namespace, id } if namespace == "" => {
            Ok(Box::new(streams::Process::new(id)))
        }
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}

fn sink(expression: parsers::Expression) -> Result<Box<dyn Sink>> {
    match expression {
        parsers::Expression::Identifier { namespace: _, id } if id == "stdout" => {
            Ok(Box::new(streams::Stdout::new()))
        }
        parsers::Expression::Identifier { namespace, id } if namespace == "file" => {
            Ok(Box::new(streams::File::new(id)))
        }
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}
