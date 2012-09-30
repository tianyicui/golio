type repl_config = {
  interactive : bool;
  print_result : bool;
  lexbuf : Lexing.lexbuf option;
  stdin : in_channel;
  stdout : out_channel;
}
;;

let repl conf =
  Runtime.init ();
  let in_c = conf.stdin in
  let out_c = conf.stdout in
  let lb =
    match conf.lexbuf with
      | Some lb -> lb
      | None -> Lexing.from_channel in_c
  in
    Port.init lb in_c out_c;
    let parse () = Parser.parse Lexer.tokens lb in
    let rec go env =
      let open Printf in
        if conf.interactive then (fprintf out_c "> "; flush out_c);
        match parse () with
          | None -> ()
          | Some sexp ->
              let env', rst = Eval.eval env sexp in
                begin match rst with
                  | Type.Undefined -> ()
                  | _ ->
                      if conf.print_result then
                        fprintf out_c "%s\n" (Print.print_value rst)
                end;
                go env';
    in
      go (Prim_env.prim_env ());
      Runtime.finish ()
;;
