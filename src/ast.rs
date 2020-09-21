use std::boxed::Box;
use std::io;

use super::errors::Result;

pub struct Program {
    pub pipe: Pipe,
}

pub struct Pipe {
    pub source: Box<dyn Source>,
    pub sink: Box<dyn Sink>,
}

pub trait Source {
    fn open<'a>(&'a self) -> Result<Box<dyn io::Read + 'a>>;
}

pub trait Sink {
    fn open<'a>(&'a self) -> Result<Box<dyn io::Write + 'a>>;
}
