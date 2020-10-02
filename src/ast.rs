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

impl Representable for Expression {
    fn repr(&self) -> String {
        match self {
            Self::Block(block) => block.repr(),
            Self::Pipe { source, sink } => source.repr() + " |> " + &sink.repr(),
        }
    }
}

impl Block for Expression {
    fn source<'a>(&'a self) -> Result<Box<dyn io::Read + 'a>> {
        match self {
            Self::Block(block) => block.source(),
            Self::Pipe { .. } => Block::source(self),
        }
    }

    fn sink<'a>(&'a self) -> Result<Box<dyn io::Write + 'a>> {
        match self {
            Self::Block(block) => block.sink(),
            Self::Pipe { .. } => Block::sink(self),
        }
    }
}
