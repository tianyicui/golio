let repl in_c out_c is_interactive =
    let lb = Lexing.from_channel in_c in
    let parse () = Parser.parse Lexer.tokens lb in
    let rec go () =
        let open Printf in
        begin if is_interactive then
            fprintf out_c "> ";
            flush out_c;
        end;
        match parse () with
        | None -> ()
        | Some sexp ->
            fprintf out_c "%s\n" (Sexp.print_sexp (Eval.eval sexp));
            go ()
    in go ()
;;
