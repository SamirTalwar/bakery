extern crate argh;
extern crate nom;
extern crate nom_locate;

mod ast;
mod errors;
mod parsed;
mod stages;
mod streams;

use std::fs;
use std::io;
use std::io::Read;

use argh::FromArgs;

use errors::Result;

#[derive(FromArgs)]
#[argh(description = "Cakes, not baking.")]
struct Args {
    #[argh(subcommand)]
    command: Command,
}

#[derive(FromArgs)]
#[argh(subcommand)]
enum Command {
    Run(RunCommand),
    Eval(EvalCommand),
}

#[derive(FromArgs)]
#[argh(description = "Runs a script.")]
#[argh(subcommand, name = "run")]
struct RunCommand {
    #[argh(positional)]
    script_path: Option<String>,
}

#[derive(FromArgs)]
#[argh(description = "Evaluates an inline script.")]
#[argh(subcommand, name = "eval")]
struct EvalCommand {
    #[argh(positional)]
    script_contents: String,
}

fn main() -> Result<()> {
    let args: Args = argh::from_env();
    read_input(args.command)
        .and_then(|input| stages::parser::parse(&input))
        .and_then(stages::resolver::program)
        .and_then(stages::interpreter::interpret)?;
    Ok(())
}

fn read_input(command: Command) -> Result<String> {
    match command {
        Command::Run(RunCommand { script_path: None }) => {
            let mut buffer = String::new();
            io::stdin()
                .read_to_string(&mut buffer)
                .map_err(errors::io)?;
            Ok(buffer)
        }
        Command::Run(RunCommand {
            script_path: Some(script_path),
        }) => fs::read_to_string(&script_path).map_err(errors::io),
        Command::Eval(EvalCommand { script_contents }) => Ok(script_contents),
    }
}
