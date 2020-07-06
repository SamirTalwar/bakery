use std::boxed::Box;
use std::io;

use super::ast::*;
use super::errors::Result;

pub struct Stdin {}

impl Stdin {
    pub fn new() -> Self {
        Stdin {}
    }
}

impl Source for Stdin {
    fn open(&self) -> Result<Box<dyn io::Read>> {
        Ok(Box::new(io::stdin()))
    }
}

pub struct Stdout {}

impl Stdout {
    pub fn new() -> Self {
        Stdout {}
    }
}

impl Sink for Stdout {
    fn open(&self) -> Result<Box<dyn io::Write>> {
        Ok(Box::new(io::stdout()))
    }
}
