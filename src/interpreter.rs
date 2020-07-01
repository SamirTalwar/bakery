use std::io;

use super::types::*;

pub fn interpret(_program: Program) -> io::Result<()> {
    println!("Hello, world!");
    Ok(())
}
