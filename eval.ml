open Type

module L = List
module M = Map.Make(String)

let rec eval env sexp =
  match sexp with
    | Symbol id -> (env, Env.get_var id env)
    | List lst ->
        (match L.map unpack_sexp lst with
           | hd :: tl ->
               let env', id = eval env hd in
                 (match id with
                    | Func _ ->
                        let env'', args = Eval_list.map eval env' tl in
                          env'', (Prim_func.apply eval id (list_ args))
                    | Macro (PrimMacro (_, macro)) ->
                        macro env' tl
                    | _ -> invalid_arg "eval: invalid application"
                 )
           | [] -> invalid_arg "eval: invalid application"
        )
    | DottedList _ -> invalid_arg "eval: cannot eval dotted list"
    | sexp -> env, Sexp sexp
;;
