let lex str =
    let lb = Lexing.from_string str in
    let rec run() = match (Lexer.tokens lb) with
        `Eof   -> [`Eof]
      | x -> x :: run()
    in (run())
;;

match (lex "'(abc 123)") with
    [`Quote; `LeftParenthesis; `Symbol "abc"; `Number 123; `RightParenthesis;
        `Eof] -> ()
  | _ -> assert false
