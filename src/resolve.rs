use super::ast::*;
use super::errors::{Error, Result};
use super::parsers;
use super::streams;

pub fn program(program: parsers::Program) -> Result<Program> {
    let pipe = expression(*program.expression.value)?;
    Ok(Program { pipe })
}

pub fn expression(expression: parsers::Expression) -> Result<Pipe> {
    match expression {
        command
        @
        parsers::Expression::Command {
            command: _,
            arguments: _,
        } => {
            let source = self::source(command)?;
            let sink = Box::new(streams::Stdout::new());
            Ok(Pipe { source, sink })
        }
        parsers::Expression::Pipe { source, sink } => {
            let source = self::source(*source.value)?;
            let sink = self::sink(*sink.value)?;
            Ok(Pipe { source, sink })
        }
    }
}

fn source(expression: parsers::Expression) -> Result<Box<dyn Source>> {
    match expression {
        parsers::Expression::Command {
            command: parsers::Token::Text { contents },
            arguments,
        } if arguments.is_empty() => Ok(Box::new(streams::Text::new(contents))),
        parsers::Expression::Command {
            command: parsers::Token::Identifier { namespace: _, id },
            arguments,
        } if arguments.is_empty() && id == "stdin" => Ok(Box::new(streams::Stdin::new())),
        parsers::Expression::Command {
            command: parsers::Token::Identifier { namespace, id },
            arguments,
        } if arguments.is_empty() && namespace == "file" => Ok(Box::new(streams::File::new(id))),
        parsers::Expression::Command {
            command: parsers::Token::Identifier { namespace, id },
            arguments,
        } if namespace == "" => {
            let text_arguments = arguments
                .into_iter()
                .map(|argument| match argument {
                    parsers::Token::Text { contents } => Ok(contents),
                    _ => Err(Error::InvalidArgument(argument)),
                })
                .collect::<Result<Vec<String>>>()?;
            Ok(Box::new(streams::Process::new(id, text_arguments)))
        }
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}

fn sink(expression: parsers::Expression) -> Result<Box<dyn Sink>> {
    match expression {
        parsers::Expression::Command {
            command: parsers::Token::Identifier { namespace: _, id },
            arguments,
        } if arguments.is_empty() && id == "stdout" => Ok(Box::new(streams::Stdout::new())),
        parsers::Expression::Command {
            command: parsers::Token::Identifier { namespace, id },
            arguments,
        } if arguments.is_empty() && namespace == "file" => Ok(Box::new(streams::File::new(id))),
        parsers::Expression::Command {
            command: parsers::Token::Identifier { namespace, id },
            arguments,
        } if namespace == "" => {
            let text_arguments = arguments
                .into_iter()
                .map(|argument| match argument {
                    parsers::Token::Text { contents } => Ok(contents),
                    _ => Err(Error::InvalidArgument(argument)),
                })
                .collect::<Result<Vec<String>>>()?;
            Ok(Box::new(streams::Process::new(id, text_arguments)))
        }
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}
