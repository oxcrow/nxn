//

#[derive(Default, Debug, Clone, Copy)]
pub struct Token<'code> {
    pub kind: TokenKind<'code>,
    pub span: Span,
}

#[derive(Default, Debug, Clone, Copy)]
pub enum TokenKind<'code> {
    Document(&'code str),
    Comment(&'code str),

    // Structures
    Function,

    // Commands
    Let,

    // Symbols
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Maths
    Eq,
    EqEq,

    // Types
    Unit,
    Int,

    // Values
    IntVal(&'code str),
    IdVal(&'code str),

    #[default]
    None,
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Span {
    beg: u32,
    end: u32,
}

/////////////////////////////////////////////////////////////////////

impl<'c> TokenKind<'c> {
    pub fn is_valid(&self) -> bool {
        match self {
            TokenKind::None => false,
            _ => true,
        }
    }

    pub fn is_not_valid(&self) -> bool {
        !self.is_valid()
    }
}

impl Span {
    pub fn new(beg: usize, end: usize) -> Self {
        Span {
            beg: beg as u32,
            end: end as u32,
        }
    }

    pub fn beg(&self) -> usize {
        self.beg as usize
    }

    pub fn end(&self) -> usize {
        self.end as usize
    }

    pub fn span(&self) -> usize {
        self.end() - self.beg()
    }
}
