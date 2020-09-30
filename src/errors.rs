use std::io;
use std::result;

use super::parsers::{Expression, Token};

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Parse(String),
    UnresolvedExpression(Expression),
    UninterpretableProgram,
    InvalidArgument(Token),
    InvalidSource(String),
    InvalidSink(String),
    CouldNotOpenSource(String),
    CouldNotOpenSink(String),
}

pub fn io(error: io::Error) -> Error {
    Error::IO(error)
}
