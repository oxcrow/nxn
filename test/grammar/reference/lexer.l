%option noyywrap
%option yylineno
%{
#include "parser.tab.h"
%}

%%

"fn" {return FN;}
"let" {return LET;}

"(" {return LPAREN;}
")" {return RPAREN;}
"{" {return LBRACE;}
"}" {return RBRACE;}

"==" {return EQEQ;}
"=" {return EQ;}
";" {return SEMICOLON;}

"int" {return INT;}

[0-9]+ {return INTVAL;}
[a-zA-Z_][a-zA-Z_]* {return IDVAL;}

[ \t\n] {/* ignore whitespace */}
. { fprintf(stderr, "Error: unknown character near (Line: %d)\n", yylineno); }

%%
