use super::ast;
use super::errors::{Error, Result};
use super::parsers;
use super::streams;

pub fn program(program: parsers::Program) -> Result<ast::Expression> {
    expression(*program.expression.value)
}

pub fn expression(expression: parsers::Expression) -> Result<ast::Expression> {
    match expression {
        command @ parsers::Expression::Command { .. } => {
            let source = self::source(command)?;
            let sink = ast::Expression::Block(Box::new(streams::Stdout::new()));
            Ok(ast::Expression::Pipe {
                source: Box::new(source),
                sink: Box::new(sink),
            })
        }
        parsers::Expression::Pipe { source, sink } => {
            let source = self::source(*source.value)?;
            let sink = self::sink(*sink.value)?;
            Ok(ast::Expression::Pipe {
                source: Box::new(source),
                sink: Box::new(sink),
            })
        }
    }
}

fn source(expression: parsers::Expression) -> Result<ast::Expression> {
    match expression {
        parsers::Expression::Pipe { .. } => self::expression(expression),
        parsers::Expression::Command {
            command: parsers::Token::Text { contents },
            arguments,
        } if arguments.is_empty() => Ok(ast::Expression::Block(Box::new(streams::Text::new(
            contents,
        )))),
        parsers::Expression::Command {
            command: parsers::Token::Raw { value },
            arguments,
        } if arguments.is_empty() && value == "stdin" => {
            Ok(ast::Expression::Block(Box::new(streams::Stdin::new())))
        }
        parsers::Expression::Command {
            command: parsers::Token::Identifier { namespace, id },
            arguments,
        } if arguments.is_empty() && namespace == "file" => {
            Ok(ast::Expression::Block(Box::new(streams::File::new(id))))
        }
        parsers::Expression::Command {
            command: parsers::Token::Raw { value },
            arguments,
        } => process(value, arguments),
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}

fn sink(expression: parsers::Expression) -> Result<ast::Expression> {
    match expression {
        parsers::Expression::Pipe { .. } => self::expression(expression),
        parsers::Expression::Command {
            command: parsers::Token::Raw { value },
            arguments,
        } if arguments.is_empty() && value == "stdout" => {
            Ok(ast::Expression::Block(Box::new(streams::Stdout::new())))
        }
        parsers::Expression::Command {
            command: parsers::Token::Identifier { namespace, id },
            arguments,
        } if arguments.is_empty() && namespace == "file" => {
            Ok(ast::Expression::Block(Box::new(streams::File::new(id))))
        }
        parsers::Expression::Command {
            command: parsers::Token::Raw { value },
            arguments,
        } => process(value, arguments),
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}

fn process(command: String, arguments: Vec<parsers::Token>) -> Result<ast::Expression> {
    let text_arguments = arguments
        .into_iter()
        .map(|argument| match argument {
            parsers::Token::Raw { value } => Ok(value),
            parsers::Token::Text { contents } => Ok(contents),
            parsers::Token::Identifier { namespace, id } if namespace == "file" => Ok(id),
            _ => Err(Error::InvalidArgument(argument)),
        })
        .collect::<Result<Vec<String>>>()?;
    let process = streams::Process::new(command, text_arguments);
    Ok(ast::Expression::Block(Box::new(process)))
}
