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

pub trait DynSource {
    fn as_dyn_source<'a>(self: Box<Self>) -> Box<dyn Source + 'a>
    where
        Self: 'a;
}

impl<T: Source + Sized> DynSource for T {
    fn as_dyn_source<'a>(self: Box<Self>) -> Box<dyn Source + 'a>
    where
        Self: 'a,
    {
        self
    }
}

pub trait Sink {
    fn open<'a>(&'a self) -> Result<Box<dyn io::Write + 'a>>;
}

pub trait DynSink {
    fn as_dyn_sink<'a>(self: Box<Self>) -> Box<dyn Sink + 'a>
    where
        Self: 'a;
}

impl<T: Sink + Sized> DynSink for T {
    fn as_dyn_sink<'a>(self: Box<Self>) -> Box<dyn Sink + 'a>
    where
        Self: 'a,
    {
        self
    }
}
