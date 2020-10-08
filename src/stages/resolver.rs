use super::super::ast;
use super::super::errors::{Error, Result};
use super::super::parsed;
use super::super::streams;

pub fn program(program: parsed::Program) -> Result<ast::Expression> {
    expression(*program.expression.value)
}

pub fn expression(expression: parsed::Expression) -> Result<ast::Expression> {
    match expression {
        command @ parsed::Expression::Command { .. } => {
            let source = self::source(command)?;
            let sink = ast::Expression::Block(Box::new(streams::Stdout::new()));
            Ok(ast::Expression::Pipe {
                source: Box::new(source),
                sink: Box::new(sink),
            })
        }
        parsed::Expression::Pipe { source, sink } => {
            let source = self::source(*source.value)?;
            let sink = self::sink(*sink.value)?;
            Ok(ast::Expression::Pipe {
                source: Box::new(source),
                sink: Box::new(sink),
            })
        }
    }
}

fn source(expression: parsed::Expression) -> Result<ast::Expression> {
    match expression {
        parsed::Expression::Pipe { .. } => self::expression(expression),
        parsed::Expression::Command {
            command: parsed::Token::Text { contents },
            arguments,
        } if arguments.is_empty() => Ok(ast::Expression::Block(Box::new(streams::Text::new(
            contents,
        )))),
        parsed::Expression::Command {
            command: parsed::Token::Raw { value },
            arguments,
        } if arguments.is_empty() && value == "stdin" => {
            Ok(ast::Expression::Block(Box::new(streams::Stdin::new())))
        }
        parsed::Expression::Command {
            command: parsed::Token::Identifier { namespace, id },
            arguments,
        } if arguments.is_empty() && namespace == "file" => {
            Ok(ast::Expression::Block(Box::new(streams::File::new(id))))
        }
        parsed::Expression::Command {
            command: parsed::Token::Raw { value },
            arguments,
        } => process(value, arguments),
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}

fn sink(expression: parsed::Expression) -> Result<ast::Expression> {
    match expression {
        parsed::Expression::Pipe { .. } => self::expression(expression),
        parsed::Expression::Command {
            command: parsed::Token::Raw { value },
            arguments,
        } if arguments.is_empty() && value == "stdout" => {
            Ok(ast::Expression::Block(Box::new(streams::Stdout::new())))
        }
        parsed::Expression::Command {
            command: parsed::Token::Identifier { namespace, id },
            arguments,
        } if arguments.is_empty() && namespace == "file" => {
            Ok(ast::Expression::Block(Box::new(streams::File::new(id))))
        }
        parsed::Expression::Command {
            command: parsed::Token::Raw { value },
            arguments,
        } => process(value, arguments),
        _ => Err(Error::UnresolvedExpression(expression)),
    }
}

fn process(command: String, arguments: Vec<parsed::Token>) -> Result<ast::Expression> {
    let text_arguments = arguments
        .into_iter()
        .map(|argument| match argument {
            parsed::Token::Raw { value } => Ok(value),
            parsed::Token::Text { contents } => Ok(contents),
            parsed::Token::Identifier { namespace, id } if namespace == "file" => Ok(id),
            _ => Err(Error::InvalidArgument(argument)),
        })
        .collect::<Result<Vec<String>>>()?;
    let process = streams::Process::new(command, text_arguments);
    Ok(ast::Expression::Block(Box::new(process)))
}
