use std::io;
use std::result;

use super::parsed::{Expression, Token};

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Impossible(String),
    Parse(String),
    UnresolvedExpression(Expression),
    UninterpretableProgram,
    InvalidArgument(Token),
    InvalidSource(String),
    InvalidSink(String),
    InvalidConduit(String),
}

pub fn io(error: io::Error) -> Error {
    Error::IO(error)
}
