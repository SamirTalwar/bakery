use super::ast::Program;
use super::errors;
use super::errors::Result;

pub fn interpret(program: Program) -> Result<()> {
    let mut buffer: Vec<u8> = Vec::new();
    let mut source = program.pipe.source.open()?;
    source.read_to_end(&mut buffer).map_err(errors::io)?;
    let mut sink = program.pipe.sink.open()?;
    sink.write_all(&buffer[..]).map_err(errors::io)?;
    Ok(())
}
