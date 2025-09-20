const IndexSpan = @import("../core/indexSpan.zig").IndexSpan;

pub const Token = struct {
    kind: TokenKind,
    span: IndexSpan,
};

pub const TokenKind = enum(c_int) {
    SPACE = 0,
    DOCUMENT,
    COMMENT,

    // Specifiers
    CON,
    MUT,
    SET,

    // Entities
    FN,
    ENUM,
    UNION,
    STRUCT,

    // Statements
    LET,
    RETURN,

    // Operators
    BREAK,
    CONTINUE,

    // Symbols
    LBRACE,
    RBRACE,
    LBRACK,
    RBRACK,
    LPAREN,
    RPAREN,

    // Operators
    EQUAL,
    EQUALEQUAL,
    COMMA,
    COLON,
    SEMICOLON,

    // Types
    VOID,
    INT,
    FLOAT,

    // Values
    VOIDVAL,
    INTVAL,
    FLOATVAL,
    IDVAL,

    None,
};
