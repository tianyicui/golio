open Type

module L = Type.L

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
    Env.empty
;;
