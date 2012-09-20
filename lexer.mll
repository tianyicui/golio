{
open Parser
}

let character =
    ['a'-'z' 'A'-'Z' '-' '.' '/' '_' ':' '*' '+' '=' '!' '%' '^' '~' '<' '>' '?'
    '#' '&' '\\' '|']

rule tokens = parse
    '(' { LPAREN }
  | ')' { RPAREN }
  | ''' { QUOTE }
  | ';' { comments lexbuf }
  | ['1'-'9'] ['0'-'9']* as number { NUMBER (int_of_string number) }
  | character (character | ['0'-'9'])* as symbol { SYMBOL symbol }
  | eof { EOF }
  | _ { tokens lexbuf }
and comments = parse
    '\n' { tokens lexbuf }
  | eof { EOF }
  | _ { comments lexbuf }
