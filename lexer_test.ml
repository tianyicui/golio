open Parser

let _ =
  let test str rst =
    let lb = Lexing.from_string str in
    let rec run () =
      match (Lexer.tokens lb) with
        | EOF   -> [EOF]
        | x -> x :: run ()
    in assert (run () = rst)
  in

    test "'(a-bc-2 -1024 #f . \"hello\") ; comment"
      [QUOTE; LPAREN; SYMBOL "a-bc-2"; NUMBER (-1024); BOOL false;
       DOT; STRING "hello"; RPAREN; EOF];

    test "0 -0" [NUMBER 0; NUMBER 0; EOF];

    test "`a ,-1 ,@()"
      [QUASIQUOTE ; SYMBOL "a"; UNQUOTE; NUMBER (-1); UNQUOTE_SPLICING; LPAREN; RPAREN; EOF];

    prerr_string "All passed!\n"
