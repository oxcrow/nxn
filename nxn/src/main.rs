use anyhow::Result;

#[allow(unused_variables)]
fn main() -> Result<()> {
    use nxn::lexer::lex_code;

    let code = std::fs::read_to_string("src/x.n")?;
    let lexer = lex_code(&code, vec![], vec![])?;

    nxn::bye();
    Ok(())
}
