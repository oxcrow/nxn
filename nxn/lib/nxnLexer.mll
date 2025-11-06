let whitespace = [' ''\t']
let newline = ['\n']
let digit = ['0'-'9']
let integer = digit+
let float = digit+(['.']digit+)?
let id = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*
let document = "///"[^'\n']*newline
let comment = "//"[^'\n']*newline

rule token = parse
  (* Simple symbols *)
  | document { Lexing.new_line lexbuf; token lexbuf }
  | comment { Lexing.new_line lexbuf; token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | whitespace { token lexbuf }

  (* Terminals *)
  | integer as lexeme { NxnParser.INTVAL(int_of_string lexeme) }
  | float as lexeme { NxnParser.FLOATVAL(float_of_string lexeme) }

  | "fn" { NxnParser.FN }
  | "struct" { NxnParser.STRUCT }
  | "enum" { NxnParser.ENUM }
  | "trait" { NxnParser.TRAIT }
  | "implement" { NxnParser.IMPLEMENT }

  | "let" { NxnParser.LET }
  | "con" { NxnParser.CON }
  | "mut" { NxnParser.MUT }
  | "set" { NxnParser.SET }

  | "loop" { NxnParser.LOOP }
  | "while" { NxnParser.WHILE }
  | "for" { NxnParser.FOR }
  | "if" { NxnParser.IF }
  | "else" { NxnParser.ELSE }
  | "continue" { NxnParser.CONTINUE }
  | "break" { NxnParser.BREAK }
  | "match" { NxnParser.MATCH }

  | "unsafe" { NxnParser.UNSAFE }
  | "extern" { NxnParser.EXTERN }
  | "pub" { NxnParser.PUBLIC }
  | "loc" { NxnParser.LOCAL }

  | "type" { NxnParser.TYPE }
  | "bool" { NxnParser.BOOL }
  | "int" { NxnParser.INT }
  | "float" { NxnParser.FLOAT }
  | "true" { NxnParser.TRUE }
  | "false" { NxnParser.FALSE }
  | "undefined" { NxnParser.UNDEFINED }
  | "as" { NxnParser.AS }

  | "==" { NxnParser.EQ }
  | "!=" { NxnParser.NE }
  | "+<=" { NxnParser.LE }
  | "+>=" { NxnParser.GE }
  | "+<" { NxnParser.LT }
  | "+>" { NxnParser.GT }

  | "@" { NxnParser.AT }
  | ";" { NxnParser.SEMICOLON }
  | ":" { NxnParser.COLON }
  | "," { NxnParser.COMMA }
  | ".." { NxnParser.DOTDOT }
  | "." { NxnParser.DOT }
  | "?" { NxnParser.QUESTION }
  | "!" { NxnParser.EXCLAMATION }
  | "&" { NxnParser.AMPERSAND }
  | "#" { NxnParser.HASH }
  | "%" { NxnParser.PERCENT }
  | "$" { NxnParser.DOLLAR }
  | "=" { NxnParser.EQUAL }
  | "{" { NxnParser.LBRACE }
  | "}" { NxnParser.RBRACE }
  | "(" { NxnParser.LPAREN }
  | ")" { NxnParser.RPAREN }
  | "[" { NxnParser.LBRACK }
  | "[" { NxnParser.LBRACK }
  | "<" { NxnParser.LANGLE }
  | ">" { NxnParser.RANGLE }
  | "+" { NxnParser.PLUS }
  | "-" { NxnParser.MINUS }
  | "*" { NxnParser.STAR }
  | "/" { NxnParser.SLASH }
  | "^" { NxnParser.CARET }
  | "|" { NxnParser.BAR }

  | "return" { NxnParser.RETURN}

  (* Identifiers *)
  | id as lexeme { NxnParser.IDVAL(lexeme) }

  (* Catch'em all! *)
  | eof { NxnParser.EOF }
  | _ { raise (Failure ("Unknown character: " ^ Lexing.lexeme lexbuf)) }
