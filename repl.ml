let _ =
    let lb = Lexing.from_channel stdin in
    let parse () = Parser.parse Lexer.tokens lb in
    while true do
        let open Printf in
        printf ">>> ";
        flush stdout;
        match parse () with
        | None -> exit 0
        | Some sexp ->
            let open Helper in
            assert (parse_str (print_sexp sexp) = [sexp]);
            printf "%s\n" (print_sexp (Eval.eval sexp))
    done
