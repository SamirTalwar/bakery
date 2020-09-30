use std::boxed::Box;
use std::io;

use super::errors::Result;

pub enum Expression {
    Block(Box<dyn Block>),
    Pipe {
        source: Box<Expression>,
        sink: Box<Expression>,
    },
}

pub trait Block {
    fn source<'a>(&'a self) -> Result<Box<dyn io::Read + 'a>>;

    fn sink<'a>(&'a self) -> Result<Box<dyn io::Write + 'a>>;
}
