use super::ast::Program;
use super::errors;
use super::errors::Result;

pub fn interpret(mut program: Program) -> Result<()> {
    let mut buffer: Vec<u8> = Vec::new();
    program
        .pipe
        .source
        .0
        .read_to_end(&mut buffer)
        .map_err(errors::io)?;
    program
        .pipe
        .sink
        .0
        .write_all(&mut buffer[..])
        .map_err(errors::io)?;
    Ok(())
}
