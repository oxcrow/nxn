use anyhow::Context;
use anyhow::Result;
//
use crate::lexer::Span;
use crate::lexer::Token;
use crate::lexer::TokenKind;

#[derive(Default, Debug, Clone)]
pub struct Lexer<'code> {
    pub code: &'code str,
    pub kinds: Vec<TokenKind<'code>>,
    pub spans: Vec<Span>,
}

/////////////////////////////////////////////////////////////////////

pub fn lex_code<'c>(
    code: &'c str,
    mut kinds: Vec<TokenKind<'c>>,
    mut spans: Vec<Span>,
) -> Result<Lexer<'c>> {
    kinds.clear();
    spans.clear();

    let mut rest = code;
    let mut ichar = 0;

    while !rest.is_empty() {
        // Lex token
        let (token, last) = lex_word(rest, ichar)?;

        // Store token
        if token.kind.is_valid() {
            kinds.push(token.kind);
            spans.push(token.span);
        }

        // Skip
        rest = last;
        ichar = token.span.end();
    }

    Ok(Lexer { code, kinds, spans })
}

pub fn lex_word(code: &str, ichar: usize) -> Result<(Token, &str)> {
    let token = {
        let c = code.chars().next().unwrap_or(' ');
        let d = code.chars().nth(1).unwrap_or(' ');
        let e = code.chars().nth(2).unwrap_or(' ');
        let f = code.chars().nth(3).unwrap_or(' ');

        let spanx = |n: usize| Span::new(ichar, ichar + n);
        let span1 = spanx(1);
        let span2 = spanx(2);
        let span3 = spanx(3);

        let none = Token {
            kind: TokenKind::None,
            span: Span::new(ichar, ichar + 1),
        };

        let (kind, span) = match c {
            '/' => {
                match (c, d, e) {
                    ('/', '/', '/') => lexdocument(code, ichar).unwrap_or((none.kind, none.span)),
                    ('/', '/', _) => lexcomment(code, ichar).unwrap_or((none.kind, none.span)),
                    _ => (none.kind, none.span), //
                }
            }
            '(' => {
                (TokenKind::Lparen, span1) //
            }
            ')' => {
                (TokenKind::Rparen, span1) //
            }
            '{' => {
                (TokenKind::Lbrace, span1) //
            }
            '}' => {
                (TokenKind::Rbrace, span1) //
            }
            '=' => {
                match (c, d, isdel(e)) {
                    ('=', '=', true) => (TokenKind::EqEq, span2),
                    ('=', _, _) => (TokenKind::Eq, span1),
                    _ => (none.kind, none.span), //
                }
            }
            'f' => {
                match (c, d, isdel(e)) {
                    ('f', 'n', true) => (TokenKind::Function, span2),
                    _ => (none.kind, none.span), //
                }
            }
            'i' => {
                match (c, d, e, isdel(f)) {
                    ('i', 'n', 't', true) => (TokenKind::Int, span3),
                    _ => (none.kind, none.span), //
                }
            }
            'l' => {
                match (c, d, e, isdel(f)) {
                    ('l', 'e', 't', true) => (TokenKind::Let, span3),
                    _ => (none.kind, none.span), //
                }
            }
            _ => (none.kind, none.span),
        };

        let (kind, span) = {
            if kind.is_not_valid() {
                if isnum(c) {
                    lexint(code, ichar).unwrap_or((none.kind, none.span))
                } else if isalpha(c) || c == '_' {
                    lexid(code, ichar).unwrap_or((none.kind, none.span))
                } else {
                    (none.kind, none.span)
                }
            } else {
                (kind, span)
            }
        };

        let token = Token { kind, span };
        token
    };

    let rest = code
        .get(token.span.span()..)
        .context("access out of bounds while slicing rest of the code")?;

    Ok((token, rest))
}

fn lexdocument<'c>(code: &'c str, ichar: usize) -> Option<(TokenKind<'c>, Span)> {
    let iend = code.chars().position(|c| c == '\n').unwrap_or(code.len());
    let kind = TokenKind::Document(code.get(0..iend).unwrap());
    let span = Span::new(ichar, ichar + iend);
    Some((kind, span))
}

fn lexcomment<'c>(code: &'c str, ichar: usize) -> Option<(TokenKind<'c>, Span)> {
    let iend = code.chars().position(|c| c == '\n').unwrap_or(code.len());
    let kind = TokenKind::Comment(code.get(0..iend).unwrap());
    let span = Span::new(ichar, ichar + iend);
    Some((kind, span))
}

fn lexint<'c>(code: &'c str, ichar: usize) -> Option<(TokenKind<'c>, Span)> {
    let iend = code.chars().position(|c| isdel(c)).unwrap_or(code.len());
    let kind = TokenKind::IntVal(code.get(0..iend).unwrap());
    let span = Span::new(ichar, ichar + iend);
    Some((kind, span))
}

fn lexid<'c>(code: &'c str, ichar: usize) -> Option<(TokenKind<'c>, Span)> {
    let iend = code
        .chars()
        .position(|c| !(isalpha(c) || isnum(c) || c == '_'))
        .unwrap_or(1);
    let kind = TokenKind::IdVal(code.get(0..iend).unwrap());
    let span = Span::new(ichar, ichar + iend);
    Some((kind, span))
}

fn isdel(c: char) -> bool {
    match c {
        '0'..='9' => false,
        'a'..='z' => false,
        'A'..='Z' => false,
        _ => true,
    }
}

fn isnum(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

fn isalpha(c: char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        _ => false,
    }
}
