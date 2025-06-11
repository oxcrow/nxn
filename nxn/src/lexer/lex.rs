use anyhow::Result;

use super::Token;
use super::TokenKind;

pub fn lex_code(mut code: &str) -> Result<()> {
    let mut icode = 0;
    let mut tokens = Vec::<Token>::new();

    while !code.is_empty() {
        let (token, rest) = lex_word(code, icode)?;

        if token.kind != TokenKind::None {
            tokens.push(token);
        }

        icode = token.span[1] as usize;
        code = rest;
    }

    Ok(())
}

pub fn lex_word(code: &str, icode: usize) -> Result<(Token, &str)> {
    let none = Token {
        kind: TokenKind::None,
        span: [icode as u32, icode as u32 + 1],
    };

    let span1 = [icode as u32, icode as u32 + 1];
    let span2 = [icode as u32, icode as u32 + 2];
    let span3 = [icode as u32, icode as u32 + 3];

    let rest = &code[1..];
    Ok((none, rest))
}
