extern crate argh;
extern crate nom;
extern crate nom_locate;

mod ast;
mod errors;
mod interpreter;
mod parsers;
mod resolve;
mod streams;

use std::fs;
use std::io;
use std::io::Read;

use argh::FromArgs;

use errors::Result;

#[derive(FromArgs)]
#[argh(description = "Cakes, not baking.")]
struct Args {
    #[argh(positional)]
    script: Option<String>,
}

fn main() -> Result<()> {
    let args: Args = argh::from_env();
    let input = match args.script {
        None => {
            let mut buffer = String::new();
            io::stdin()
                .read_to_string(&mut buffer)
                .map_err(errors::io)?;
            Ok(buffer)
        }
        Some(path) => fs::read_to_string(&path).map_err(errors::io),
    }?;
    let parsed_program = parsers::parse(&input)?;
    let resolved_program = resolve::program(parsed_program)?;
    interpreter::interpret(&resolved_program)?;
    Ok(())
}
