use std::boxed::Box;
use std::io;

use super::errors::{Error, Result};

pub trait Representable {
    fn repr(&self) -> String;
}

pub enum Expression {
    Block(Box<dyn Block>),
    Pipe {
        source: Box<Expression>,
        sink: Box<Expression>,
    },
}

pub struct Output<'a>(Box<dyn io::Write + 'a>);

impl<'a> Output<'a> {
    pub fn new<T: io::Write + 'a>(write: T) -> Output<'a> {
        Output(Box::new(io::BufWriter::new(write)))
    }
}

impl io::Write for Output<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

pub trait Block: Representable {
    fn source(&self, _next: Output) -> Result<()> {
        Err(Error::InvalidSource(self.repr()))
    }

    fn sink(&self) -> Result<Output> {
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
    fn source(&self, next: Output) -> Result<()> {
        match self {
            Self::Block(block) => block.source(next),
            Self::Pipe { .. } => Block::source(self, next),
        }
    }

    fn sink(&self) -> Result<Output> {
        match self {
            Self::Block(block) => block.sink(),
            Self::Pipe { .. } => Block::sink(self),
        }
    }
}
