use std::fs;
use std::io;
use std::io::Write;
use std::process;

use super::ast::{Block, Output, Representable};
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
    fn source(&self, mut next: Output) -> Result<()> {
        next.write(self.contents.as_bytes()).map_err(errors::io)?;
        Ok(())
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
    fn source(&self, mut next: Output) -> Result<()> {
        io::copy(&mut io::stdin(), &mut next).map_err(errors::io)?;
        Ok(())
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
    fn sink(&self) -> Result<Output> {
        Ok(Output::StdOut(io::stdout()))
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
    fn source(&self, mut next: Output) -> Result<()> {
        let mut file = fs::File::open(&self.path).map_err(errors::io)?;
        io::copy(&mut file, &mut next).map_err(errors::io)?;
        Ok(())
    }

    fn sink(&self) -> Result<Output> {
        let file = fs::File::create(&self.path).map_err(errors::io)?;
        Ok(Output::File(file))
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

    fn spawn(&self, source: bool, next: Option<Output>) -> Result<process::Child> {
        let stdin = if source {
            process::Stdio::inherit()
        } else {
            process::Stdio::piped()
        };
        let stdout = match next {
            None | Some(Output::StdOut(_)) => process::Stdio::inherit(),
            Some(Output::File(file)) => process::Stdio::from(file),
            Some(Output::Pipe(stdin)) => process::Stdio::from(stdin),
        };
        process::Command::new(&self.command)
            .args(&self.arguments)
            .stdin(stdin)
            .stdout(stdout)
            .spawn()
            .map_err(errors::io)
    }
}

impl Representable for Process {
    fn repr(&self) -> String {
        "process".to_string()
    }
}

impl Block for Process {
    fn source(&self, next: Output) -> Result<()> {
        let mut child = self.spawn(true, Some(next))?;
        child.wait().map_err(errors::io)?;
        Ok(())
    }

    fn conduit(&self, next: Output) -> Result<Output> {
        let child = self.spawn(false, Some(next))?;
        match child.stdin {
            None => Err(Error::Impossible(
                "Process STDIN is missing for conduit".to_string(),
            )),
            Some(stdin) => Ok(Output::Pipe(stdin)),
        }
    }

    fn sink(&self) -> Result<Output> {
        let child = self.spawn(false, None)?;
        match child.stdin {
            None => Err(Error::Impossible(
                "Process STDIN is missing for sink".to_string(),
            )),
            Some(stdin) => Ok(Output::Pipe(stdin)),
        }
    }
}
