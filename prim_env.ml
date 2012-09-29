open Type

module L = List

let prim_env () =
  let env_from_assoc_list f init lst =
    Env.bind_vars init (L.map (fun (k, v) -> (k, f k v)) lst)
  in
  let env_with_port =
    env_from_assoc_list
      (fun k v -> v)
      Env.empty
      (Port.prim_ports ())
  in
  let env_with_func =
    env_from_assoc_list
      (fun k v -> prim_func k v)
      env_with_port
      Prim_func.prim_functions
  in
  let env =
    env_from_assoc_list
      (fun k v -> Macro (PrimMacro (k, v)))
      env_with_func
      Prim_macro.prim_macros
  in
    fst (Prim_macro.load env [String "stdlib.scm"])
;;
