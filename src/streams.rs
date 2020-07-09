use std::boxed::Box;
use std::fs;
use std::io;

use super::ast::*;
use super::errors;
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

pub struct File {
    path: String,
}

impl File {
    pub fn new(path: String) -> Self {
        File { path }
    }
}

impl Sink for File {
    fn open(&self) -> Result<Box<dyn io::Write>> {
        let file = fs::File::create(&self.path).map_err(errors::io)?;
        Ok(Box::new(file))
    }
}
