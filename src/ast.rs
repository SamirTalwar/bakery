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

#[derive(Debug)]
pub enum Output {
    StdOut(io::Stdout),
    File(std::fs::File),
    Pipe(std::process::ChildStdin),
}

impl io::Write for Output {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            Self::StdOut(stdout) => stdout.write(buf),
            Self::File(file) => file.write(buf),
            Self::Pipe(process_stdin) => process_stdin.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            Self::StdOut(stdout) => stdout.flush(),
            Self::File(file) => file.flush(),
            Self::Pipe(process_stdin) => process_stdin.flush(),
        }
    }
}

pub trait Block: Representable {
    fn source(&self, _next: Output) -> Result<()> {
        Err(Error::InvalidSource(self.repr()))
    }

    fn conduit(&self, _next: Output) -> Result<Output> {
        Err(Error::InvalidConduit(self.repr()))
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
            Self::Pipe { source, sink } => {
                let conduit = sink.conduit(next)?;
                source.source(conduit)
            }
        }
    }

    fn conduit(&self, next: Output) -> Result<Output> {
        match self {
            Self::Block(block) => block.conduit(next),
            Self::Pipe { source, sink } => {
                let conduit = sink.conduit(next)?;
                source.conduit(conduit)
            }
        }
    }

    fn sink(&self) -> Result<Output> {
        match self {
            Self::Block(block) => block.sink(),
            Self::Pipe { source, sink } => {
                let conduit = sink.sink()?;
                source.conduit(conduit)
            }
        }
    }
}
