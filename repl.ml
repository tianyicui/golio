let repl in_c out_c is_interactive =
  let lb = Lexing.from_channel in_c in
    Port.init lb in_c out_c;
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
                  | Type.Undefined -> ()
                  | _ -> fprintf out_c "%s\n" (Print.print_value rst)
                end;
                go env';
    in go (Eval.prim_env Eval.eval)
;;
