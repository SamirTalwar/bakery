extern crate nom;
extern crate nom_locate;

mod ast;
mod errors;
mod interpreter;
mod parsers;
mod resolve;
mod streams;

use std::env;
use std::fs;
use std::io;
use std::io::Read;

use errors::{Error, Result};

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let input = match args.len() {
        1 => {
            let mut buffer = String::new();
            io::stdin()
                .read_to_string(&mut buffer)
                .map_err(errors::io)?;
            Ok(buffer)
        }
        2 => fs::read_to_string(&args[1]).map_err(errors::io),
        _ => Err(Error::InvalidArguments),
    }?;
    let parsed_program = parsers::parse(&input)?;
    let resolved_program = resolve::program(parsed_program)?;
    interpreter::interpret(&resolved_program)?;
    Ok(())
}
