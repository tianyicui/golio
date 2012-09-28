open Repl

let _ =
  let usage () =
    Printf.printf "usage: %s [-c cmd | file | - ]\n" Sys.argv.(0)
  in
    match Sys.argv with
      | [| _; "-h" |]
      | [| _; "--help" |] ->
          usage ()
      | [| _; "-c" ; sexp |] ->
          repl {
            stdin;
            stdout;
            lexbuf = Some (Lexing.from_string sexp);
            interactive = false;
            print_result = false;
          }
      | [| _; "-" |] ->
          repl {
            stdin;
            stdout;
            lexbuf = None;
            interactive = false;
            print_result = false;
          }
      | [| _; filename |] ->
          ignore (Prim_macro.load
                    Eval.eval
                    (Prim_env.prim_env Eval.eval)
                    [Type.String filename])
      | [| _ |] ->
          repl {
            stdin;
            stdout;
            lexbuf = None;
            interactive = true;
            print_result = true;
          }
      | _ -> usage ()
;;
