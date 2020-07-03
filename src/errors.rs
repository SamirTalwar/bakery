use std::io;
use std::result;

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    InvalidArguments,
    IOError(io::Error),
    ParseError(String),
}

pub fn io(error: io::Error) -> Error {
    Error::IOError(error)
}
