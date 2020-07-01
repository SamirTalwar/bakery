#[macro_use]
extern crate nom;

use std::env;
use std::fs;
use std::io;
use std::io::Read;

mod interpreter;
mod parsers;
mod types;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let input = match args.len() {
        0 => Err(io::Error::new(io::ErrorKind::Other, "Invalid invocation.")),
        1 => {
            let mut buffer = String::new();
            io::stdin().read_to_string(&mut buffer)?;
            Ok(buffer)
        }
        2 => fs::read_to_string(&args[1]),
        _ => Err(io::Error::new(io::ErrorKind::Other, "Too many arguments.")),
    }?;
    let program = parsers::program(&input)
        .map(|(_, result)| result)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
    interpreter::interpret(program)?;
    Ok(())
}
