let repl in_c out_c is_interactive =
    let lb = Lexing.from_channel in_c in
    let parse () = Parser.parse Lexer.tokens lb in
    let rec go env =
        let open Printf in
        begin if is_interactive then
            fprintf out_c "> ";
            flush out_c;
        end;
        match parse () with
        | None -> ()
        | Some sexp ->
            let env', rst = Eval.eval env sexp in
            begin match rst with
            | Types.Undefined -> ()
            | _ -> fprintf out_c "%s\n" (Sexp.print_sexp rst)
            end;
            go env';
    in go Env.empty_env
;;
