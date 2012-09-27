open Type

module L = List
module M = Map.Make(String)

let prim_env eval =
  let env_from_assoc_list f init lst =
    Env.bind_vars init (L.map (fun (k, v) -> (k, f k v)) lst)
  in
  let env_with_func =
    env_from_assoc_list
      (fun k v -> Func (PrimFunc (k, v)))
      Env.empty
      (Prim_func.prim_functions eval)
  in env_with_func
    (*
  let env =
    env_from_assoc_list
      (fun k v -> PrimitiveMacro (k, v))
      env_with_func
      (Lazy.force lazy_prim_macros)
  in env
     *)

let rec eval_list env value_list =
  let env, rst_lst = L.fold_left
                       (fun (env', lst) sexp ->
                          let env'',rst = eval env' sexp in
                            env'', (rst :: lst))
                       (env, [])
                       value_list
  in env, (L.rev rst_lst)

and eval env value =
  match unpack_sexp value with
    | Symbol id -> (env, Env.get_var id env)
    | List (hd :: tl) ->
        let env', id = eval env hd in
          (match id with
             | Func _ ->
                 let env'', args = eval_list env' tl in
                   env'', (Prim_func.apply eval id (list_ args))
             | Macro (PrimMacro (_, macro)) ->
                 macro env' tl
             | _ -> invalid_arg "eval: invalid application"
          )
    | List [] -> invalid_arg "eval: invalid application"
    | DottedList _ -> invalid_arg "eval: cannot eval dotted list"
    | sexp -> env, Sexp sexp
;;
