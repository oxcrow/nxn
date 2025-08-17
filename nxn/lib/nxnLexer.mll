let whitespace = [' ''\t']
let newline = ['\n']
let digit = ['0'-'9']
let integer = ['-''+']? digit+
let id = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*
let comment = "//"[^'\n']*newline

rule token = parse
  (* Simple symbols *)
  | comment { Lexing.new_line lexbuf; token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | whitespace { token lexbuf }

  (* Terminals *)
  | integer as lexeme { NxnParser.INTVAL(int_of_string lexeme) }

  | "fn" { NxnParser.FN }
  | "let" { NxnParser.LET }
  | "con" { NxnParser.CON }
  | "mut" { NxnParser.MUT }
  | "set" { NxnParser.SET }

  | "int" { NxnParser.INT }

  | ";" { NxnParser.SEMICOLON }
  | ":" { NxnParser.COLON }
  | "," { NxnParser.COMMA }
  | ".." { NxnParser.DOTDOT }
  | "." { NxnParser.DOT }
  | "?" { NxnParser.QUESTION }
  | "!" { NxnParser.EXCLAMATION }
  | "#" { NxnParser.HASH }
  | "=" { NxnParser.EQUAL }
  | "{" { NxnParser.LBRACE }
  | "}" { NxnParser.RBRACE }
  | "(" { NxnParser.LPAREN }
  | ")" { NxnParser.RPAREN }
  | "+" { NxnParser.PLUS }
  | "-" { NxnParser.MINUS }
  | "*" { NxnParser.STAR }
  | "/" { NxnParser.SLASH }

  | "return" { NxnParser.RETURN}

  (* Identifiers *)
  | id as lexeme { NxnParser.IDVAL(lexeme) }

  (* Catch'em all! *)
  | eof { NxnParser.EOF }
  | _ { raise (Failure ("Unknown character: " ^ Lexing.lexeme lexbuf)) }
