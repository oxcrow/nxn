#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum TokenKind<'code> {
    Document(&'code str),
    Comment(&'code str),

    Fn,
    Let,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    EqualEqual,
    Equal,
    Semicolon,

    Unit,
    Int,

    IntVal(&'code str),
    IdVal(&'code str),

    #[default]
    None,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Token<'code> {
    pub kind: TokenKind<'code>,
    pub span: [u32; 2],
}
