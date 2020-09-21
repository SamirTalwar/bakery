use std::io;
use std::result;

use super::parsers::Expression;

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    InvalidArguments,
    IO(io::Error),
    Parse(String),
    UnresolvedExpression(Expression),
    CouldNotOpenSource(String),
}

pub fn io(error: io::Error) -> Error {
    Error::IO(error)
}
