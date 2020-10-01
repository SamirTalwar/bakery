use std::boxed::Box;
use std::io;

use super::errors::{Error, Result};

pub enum Expression {
    Block(Box<dyn Block>),
    Pipe {
        source: Box<Expression>,
        sink: Box<Expression>,
    },
}

pub trait Representable {
    fn repr(&self) -> String;
}

pub trait Block: Representable {
    fn source<'a>(&'a self) -> Result<Box<dyn io::Read + 'a>> {
        Err(Error::InvalidSource(self.repr()))
    }

    fn sink<'a>(&'a self) -> Result<Box<dyn io::Write + 'a>> {
        Err(Error::InvalidSink(self.repr()))
    }
}
