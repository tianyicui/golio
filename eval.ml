open Type

module L = List
module M = Map.Make(String)

let prim_env eval =
  let env_from_assoc_list f init lst =
    Env.bind_vars init (L.map (fun (k, v) -> (k, f k v)) lst)
  in
  let env_with_func =
    env_from_assoc_list
      (fun k v -> prim_func k v)
      Env.empty
      (Prim_func.prim_functions eval)
  in
  let env =
    env_from_assoc_list
      (fun k v -> Macro (PrimMacro (k, v)))
      env_with_func
      (Prim_macro.prim_macros eval)
  in env

let rec eval env value =
  match unpack_sexp value with
    | Symbol "quote" -> env, Macro (PrimMacro ("quote", (fun e l -> (e, L.hd l))))
    | Symbol id -> (env, Env.get_var id env)
    | List (hd :: tl) ->
        let env', id = eval env hd in
          (match id with
             | Func _ ->
                 let env'', args = Eval_list.eval_list eval env' tl in
                   env'', (Prim_func.apply eval id (list_ args))
             | Macro (PrimMacro (_, macro)) ->
                 macro env' tl
             | _ -> invalid_arg "eval: invalid application"
          )
    | List [] -> invalid_arg "eval: invalid application"
    | DottedList _ -> invalid_arg "eval: cannot eval dotted list"
    | sexp -> env, Sexp sexp
;;
