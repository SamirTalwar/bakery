extern crate nom;
extern crate nom_locate;

mod errors;
mod interpreter;
mod parsers;

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
    let program = parsers::parse(&input)?;
    interpreter::interpret(program)?;
    Ok(())
}
