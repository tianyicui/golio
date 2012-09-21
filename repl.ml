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
            let str = Helper.print_sexp sexp in
            assert (Helper.parse_str str = [sexp]);
            printf "%s\n" str
    done
