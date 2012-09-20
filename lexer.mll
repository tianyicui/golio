rule tokens = parse
    '(' { `LeftParenthesis }
  | ')' { `RightParenthesis }
  | ''' { `Quote }
  | ['1'-'9'] ['0'-'9']* as number { `Number (int_of_string number) }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-']+ as symbol { `Symbol symbol }
  | eof { `Eof }
  | _ { tokens lexbuf }
