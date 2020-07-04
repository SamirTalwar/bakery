use std::boxed::Box;
use std::io;

use super::ast::*;

pub fn stdin() -> Source {
    Source(Box::new(io::stdin()))
}

pub fn stdout() -> Sink {
    Sink(Box::new(io::stdout()))
}
