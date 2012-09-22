{
open Parser
}

let character =
    ['a'-'z' 'A'-'Z' '!' '#' '$' '%' '&' '|' '*' '+' '-' '/' ':' '<' '=' '>' '?'
        '@' '^' '_' '~']

rule tokens = parse
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ''' { QUOTE }
  | '.' { DOT }
  | ';' { comments lexbuf }
  | '-'? ['1'-'9'] ['0'-'9']* as number { NUMBER (int_of_string number) }
  | '"' (([^'"''\\'] | ('\\' _))* as str) '"' {
      Scanf.sscanf ("\"" ^ str ^ "\"") "%S%!" (fun s -> STRING s)
    }
  | character (character | ['0'-'9'])* as symbol {
      match symbol with
      | "#t" -> BOOL true
      | "#f" -> BOOL false
      | _    -> SYMBOL symbol
    }
  | eof { EOF }
  | (' ' | '\t' | '\r' | '\n') { tokens lexbuf }
  | _ { raise Parsing.Parse_error }
and comments = parse
  |  '\n' { tokens lexbuf }
  | eof { EOF }
  | _ { comments lexbuf }
