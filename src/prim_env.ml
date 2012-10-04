open Type
include Common

let prim_env () =
  let map_and_bind f lst =
    Env.bind_globals (L.map (fun (k, v) -> (k, f k v)) lst)
  in
    map_and_bind
      (fun k v -> v)
      (Port.prim_ports ());
    map_and_bind
      (fun k v -> prim_func k v)
      Prim_func.prim_functions;
    map_and_bind
      (fun k v -> Macro (PrimMacro (k, v)))
      Prim_macro.prim_macros;
    let lb = Lexing.from_string Stdlib.stdlib in
    let parse () = Parser.parse Lexer.tokens lb in
    let rec go env =
      match parse () with
        | None -> env
        | Some sexp -> go (fst (Eval.eval env sexp))
    in go Env.empty
;;
