%{
open Type
%}

%token LPAREN RPAREN QUOTE DOT QUASIQUOTE UNQUOTE UNQUOTE_SPLICING EOF
%token <int> NUMBER
%token <string> SYMBOL
%token <string> STRING
%token <bool> BOOL
%start parse
%type<Type.sexp option> parse
%%

parse:
  | EOF { None }
  | expr { Some $1 }
;

exprs:
  | expr exprs { $1 :: $2 }
  | expr { [$1] }
;

expr:
  | NUMBER { Number $1 }
  | SYMBOL { Symbol $1 }
  | STRING { String $1 }
  | BOOL { Bool $1 }
  | LPAREN RPAREN { List [] }
  | LPAREN exprs RPAREN
    { List (L.map (fun s -> Sexp s) $2) }
  | LPAREN exprs DOT expr RPAREN
    { DottedList ((L.map (fun s -> Sexp s) $2), Sexp $4) }
  | QUOTE expr
    { List [Sexp (Symbol "quote") ; Sexp $2] }
  | QUASIQUOTE expr
    { List [Sexp (Symbol "quasiquote") ; Sexp $2] }
  | UNQUOTE expr
    { List [Sexp (Symbol "unquote") ; Sexp $2] }
  | UNQUOTE_SPLICING expr
    { List [Sexp (Symbol "unquote-splicing") ; Sexp $2] }
;
