use anyhow::Result;

fn main() -> Result<()> {
    let code = std::fs::read_to_string("x.n")?;
    nxn::lexer::lex_code(&code)?;
    nxn::bye();
    Ok(())
}
