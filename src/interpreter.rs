use std::io;

use super::parsers::Program;

pub fn interpret(_program: Program) -> io::Result<()> {
    println!("Hello, world!");
    Ok(())
}
