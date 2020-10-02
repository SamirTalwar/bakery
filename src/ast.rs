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
