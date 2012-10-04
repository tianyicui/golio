open Type
include Common

let rec apply func args =
  let params_len = L.length func.params in
  let args_len = L.length args in
  let env =
    (match func.vararg with
       | None ->
           if params_len == args_len then
             Env.bind_locals func.closure (List.combine func.params args)
           else
             arg_count_mismatch (string_of_int params_len) args_len
       | Some vararg ->
           if params_len <= args_len then
             let rec go params args =
               begin match params with
                 | [] -> Env.def_local vararg (list_ args) func.closure
                 | x :: xs -> Env.def_local x (L.hd args) (go xs (L.tl args))
               end
             in go func.params args
             else
               arg_count_mismatch (string_of_int params_len ^ "+") args_len
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
  match sexp_list with
    | [] -> env, Void
    | [sexp] -> eval ~tail:true env sexp
    | x :: xs ->
        let env', _ = eval env x in
          eval_all env' xs

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
                      | _ -> not_applicable id
                   )
             | [] -> not_applicable Void
          )
      | DottedList _ -> arg_type_mismatch "atom or list" (Sexp sexp)
      | sexp -> env, Sexp sexp
  in
  let rec expand_thunk value =
    match value with
      | Thunk (func, args) ->
          expand_thunk (apply func args)
      | _ -> value
  in
    if tail && !Runtime.Eval.tco then rst else fst rst, expand_thunk (snd rst)
;;
