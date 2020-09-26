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
            command: parsers::Token::Raw { value },
            arguments,
        } if arguments.is_empty() && value == "stdin" => Ok(Box::new(streams::Stdin::new())),
        parsers::Expression::Command {
            command: parsers::Token::Identifier { namespace, id },
            arguments,
        } if arguments.is_empty() && namespace == "file" => Ok(Box::new(streams::File::new(id))),
        parsers::Expression::Command {
            command: parsers::Token::Raw { value },
            arguments,
        } => process(value, arguments).map(|p| Box::new(p).as_dyn_source()),
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}

fn sink(expression: parsers::Expression) -> Result<Box<dyn Sink>> {
    match expression {
        parsers::Expression::Command {
            command: parsers::Token::Raw { value },
            arguments,
        } if arguments.is_empty() && value == "stdout" => Ok(Box::new(streams::Stdout::new())),
        parsers::Expression::Command {
            command: parsers::Token::Identifier { namespace, id },
            arguments,
        } if arguments.is_empty() && namespace == "file" => Ok(Box::new(streams::File::new(id))),
        parsers::Expression::Command {
            command: parsers::Token::Raw { value },
            arguments,
        } => process(value, arguments).map(|p| Box::new(p).as_dyn_sink()),
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}

fn process(command: String, arguments: Vec<parsers::Token>) -> Result<streams::Process> {
    let text_arguments = arguments
        .into_iter()
        .map(|argument| match argument {
            parsers::Token::Raw { value } => Ok(value),
            parsers::Token::Text { contents } => Ok(contents),
            _ => Err(Error::InvalidArgument(argument)),
        })
        .collect::<Result<Vec<String>>>()?;
    Ok(streams::Process::new(command, text_arguments))
}
