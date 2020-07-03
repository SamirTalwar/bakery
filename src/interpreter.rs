use super::errors::Result;
use super::parsers::Program;

pub fn interpret(_program: Program) -> Result<()> {
    println!("Hello, world!");
    Ok(())
}
