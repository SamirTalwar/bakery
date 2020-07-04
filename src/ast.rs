use std::boxed::Box;
use std::io;

pub struct Program {
    pub pipe: Pipe,
}

pub struct Pipe {
    pub source: Source,
    pub sink: Sink,
}

pub struct Source(pub Box<dyn io::Read>);

pub struct Sink(pub Box<dyn io::Write>);
