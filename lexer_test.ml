open Parser

let lex str =
    let lb = Lexing.from_string str in
    let rec run() = match (Lexer.tokens lb) with
        EOF   -> [EOF]
      | x -> x :: run()
    in (run())
;;

match (lex "'(a-bc-2 1024)") with
    [QUOTE; LPAREN; SYMBOL "a-bc-2"; NUMBER 1024; RPAREN; EOF] -> ()
;

Printf.printf "All passed!\n"
