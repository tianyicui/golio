open Parser

let lex str =
    let lb = Lexing.from_string str in
    let rec run() =
        match (Lexer.tokens lb) with
        | EOF   -> [EOF]
        | x -> x :: run()
    in (run())
;;


let _ =
    let test str rst =
        assert (lex str = rst)
    in

    test "'(a-bc-2 1024 #f . \"hello\") ; comment"
        [QUOTE; LPAREN; ATOM "a-bc-2"; NUMBER 1024; BOOL false;
            DOT; STRING "hello"; RPAREN; EOF];

    Printf.printf "All passed!\n"
