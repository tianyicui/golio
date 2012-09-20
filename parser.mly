%{
open Sexp
%}

%token LPAREN RPAREN QUOTE EOF
%token <int> NUMBER
%token <string> SYMBOL
%start parse
%type< Sexp.sexp list > parse
%%

parse:
    exprs EOF { $1 }

exprs:
    expr exprs { $1 :: $2 }
  | expr { [$1] }
;

expr:
    NUMBER { Number $1 }
  | SYMBOL { Symbol $1 }
  | LPAREN exprs RPAREN { List $2 }
  | LPAREN RPAREN { List [] }
  | QUOTE expr { List [Symbol "quote" ; $2] }
