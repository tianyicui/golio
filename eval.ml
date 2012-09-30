open Type

module L = Type.L

let rec expand_thunk value =
  match value with
    | Thunk (func, args) ->
        expand_thunk (apply func args)
    | _ -> value

and apply func args =
  let params_len = L.length func.params in
  let args_len = L.length args in
  let env =
    (match func.vararg with
       | None ->
           if params_len == args_len then
             Env.bind_locals func.closure (List.combine func.params args)
           else invalid_arg ("apply: invalid number of args, expected " ^
                             string_of_int params_len ^ ", given " ^
                             string_of_int args_len)
       | Some vararg ->
           if params_len <= args_len then
             let rec go params args =
               begin match params with
                 | [] -> Env.def_local vararg (list_ args) func.closure
                 | x :: xs -> Env.def_local x (L.hd args) (go xs (L.tl args))
               end
             in go func.params args
             else invalid_arg ("apply: invalid number of args, expected " ^
                               string_of_int params_len ^ "+, given " ^
                               string_of_int args_len)
    )
  in
    snd (eval_all {env with top_level = false} func.body)

and map env sexp_list =
  let env, rst_lst =
    L.fold_left
      (fun (env', lst) sexp ->
         let env'',rst = eval env' sexp in
           env'', (rst :: lst))
      (env, [])
      sexp_list
  in env, (L.rev rst_lst)

and eval_all env sexp_list =
  L.fold_left
    (fun (env', _) sexp -> eval ~tail:true env' sexp)
    (env, Undefined)
    sexp_list

and eval ?(tail=false) env sexp =
  let rst =
    match sexp with
      | Symbol id -> (env, Env.get_var id env)
      | List lst ->
          (match L.map unpack_sexp lst with
             | hd :: tl ->
                 let env', id = eval env hd in
                   (match id with
                      | Func _ ->
                          let env'', args = map env' tl in
                            env'', Prim_func.apply [id; list_ args]
                      | Macro (PrimMacro (_, macro)) ->
                          macro env' tl
                      | _ -> invalid_arg "eval: invalid application"
                   )
             | [] -> invalid_arg "eval: invalid application"
          )
      | DottedList _ -> invalid_arg "eval: cannot eval dotted list"
      | sexp -> env, Sexp sexp
  in
    if tail && !Runtime.enable_tco
    then rst else fst rst, expand_thunk (snd rst)
;;
