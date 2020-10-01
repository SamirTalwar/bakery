use std::boxed::Box;
use std::fs;
use std::io;
use std::process;

use super::ast::*;
use super::errors;
use super::errors::{Error, Result};

#[derive(Debug)]
pub struct Text {
    contents: String,
}

impl Text {
    pub fn new(contents: String) -> Self {
        Text {
            contents: contents + "\n",
        }
    }
}

impl Representable for Text {
    fn repr(&self) -> String {
        "text".to_string()
    }
}

impl Block for Text {
    fn source<'a>(&'a self) -> Result<Box<dyn io::Read + 'a>> {
        Ok(Box::new(self.contents.as_bytes()))
    }
}

impl io::Read for Text {
    fn read(&mut self, buf: &mut [u8]) -> std::result::Result<usize, std::io::Error> {
        self.contents.as_bytes().read(buf)
    }
}

#[derive(Debug)]
pub struct Stdin {}

impl Stdin {
    pub fn new() -> Self {
        Stdin {}
    }
}

impl Representable for Stdin {
    fn repr(&self) -> String {
        "stdin".to_string()
    }
}

impl Block for Stdin {
    fn source<'a>(&'a self) -> Result<Box<dyn io::Read + 'a>> {
        Ok(Box::new(io::stdin()))
    }
}

#[derive(Debug)]
pub struct Stdout {}

impl Stdout {
    pub fn new() -> Self {
        Stdout {}
    }
}

impl Representable for Stdout {
    fn repr(&self) -> String {
        "stdout".to_string()
    }
}

impl Block for Stdout {
    fn sink<'a>(&'a self) -> Result<Box<dyn io::Write + 'a>> {
        Ok(Box::new(io::stdout()))
    }
}

#[derive(Debug)]
pub struct File {
    path: String,
}

impl File {
    pub fn new(path: String) -> Self {
        File { path }
    }
}

impl Representable for File {
    fn repr(&self) -> String {
        "file".to_string()
    }
}

impl Block for File {
    fn source<'a>(&'a self) -> Result<Box<dyn io::Read + 'a>> {
        let file = fs::File::open(&self.path).map_err(errors::io)?;
        Ok(Box::new(file))
    }

    fn sink<'a>(&'a self) -> Result<Box<dyn io::Write + 'a>> {
        let file = fs::File::create(&self.path).map_err(errors::io)?;
        Ok(Box::new(file))
    }
}

#[derive(Debug)]
pub struct Process {
    command: String,
    arguments: Vec<String>,
}

impl Process {
    pub fn new(command: String, arguments: Vec<String>) -> Self {
        Process { command, arguments }
    }
}

impl Representable for Process {
    fn repr(&self) -> String {
        "process".to_string()
    }
}

impl Block for Process {
    fn source<'a>(&'a self) -> Result<Box<dyn io::Read + 'a>> {
        let child = process::Command::new(&self.command)
            .args(&self.arguments)
            .stdout(process::Stdio::piped())
            .spawn()
            .map_err(errors::io)?;
        match child.stdout {
            None => Err(Error::CouldNotOpenSource("process".to_string())),
            Some(stdout) => Ok(Box::new(stdout)),
        }
    }

    fn sink<'a>(&'a self) -> Result<Box<dyn io::Write + 'a>> {
        let child = process::Command::new(&self.command)
            .args(&self.arguments)
            .stdin(process::Stdio::piped())
            .spawn()
            .map_err(errors::io)?;
        match child.stdin {
            None => Err(Error::CouldNotOpenSink("process".to_string())),
            Some(stdin) => Ok(Box::new(stdin)),
        }
    }
}
