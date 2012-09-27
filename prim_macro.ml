let rec quote env param =
  env, (Primitives.unary_op (fun x -> x) param)

and begin_ env params =
  match params with
    | [] -> env, Undefined
    | _ ->
        let env', lst = eval_list env params in
          env', L.hd (L.rev lst)

and if_ env params =
  let pred, conseq, alt =
    match params with
      | [x; y] -> x, y, List [Symbol "quote"; List []]
      | [x; y; z] -> x, y, z
      | _ -> invalid_arg "if: expected 2 or more arguments"
  in
    match (eval env pred) with
      | env', Bool false -> eval env' alt
      | env', _ -> eval env' conseq

and define env params =
  let named_lambda var params =
    let func = build_func env params in
    let func' = {
      func with
          closure = Env.def_var var Undefined func.closure
    } in
    let rst = Func func' in
      (Env.set_var var rst func'.closure; rst)
  in
  let var, (env', value) =
    match params with
      | [Symbol var; expr] ->
          var, eval env expr
      | List (Symbol var :: formals) :: body ->
          var, (env, named_lambda var (List formals :: body))
      | DottedList ([Symbol var], formal) :: body ->
          var, (env, named_lambda var (formal :: body))
      | _ -> invalid_arg "define: invalid arguments"
  in Env.def_var var value env', Undefined

and set env params =
  match params with
    | [Symbol var; expr] ->
        let env', value = eval env expr in
          (Env.set_var var value env; env, Undefined)
    | [_; _] -> invalid_arg "set: first argument should be a symbol"
    | _ -> invalid_arg "set: expected 2 arguments"

and let_to_apply is_rec env params =
  let name = if is_rec then "letrec" else "let" in
  match params with
    | [] -> invalid_arg (name ^ ": should have a binding list")
    | (List bindings_sexp) :: body ->
        let bindings =
          L.map
            (fun sexp ->
               match sexp with
                 | List [Symbol var; init] -> var, init
                 | _ -> invalid_arg (name ^ ": invalid binding list"))
            bindings_sexp
        in
        let vars, inits = L.split bindings in
        let env', values =
          eval_list
            (if is_rec
             then L.fold_left
                    (fun e v ->
                       if (Env.is_bound v e)
                       then Env.def_var v (Env.get_var v e) e
                       else Env.def_var v Undefined e
                    )
                    env
                    vars
             else env)
            inits
        in
        begin
        if is_rec then
          L.iter2 (fun var value -> Env.set_var var value env') vars values;
        let func = {params = vars; vararg = None; body = body; closure = env'} in
            env, apply (Func func) (List values)
        end
    | _ -> invalid_arg (name ^ ": invalid binding list")

and let_ env params =
  let_to_apply false env params

and letrec env params =
  let_to_apply true env params

and let_star env params =
  match params with
    | [] -> invalid_arg "let*: should have a binding list"
    | (List []) :: body ->
        begin_ env body
    | (List (first_binding :: remaining_bindings) :: body) ->
        let_ env [List [first_binding];
                  List (Symbol "let*" ::
                        List remaining_bindings ::
                        body)]
    | _ -> invalid_arg "let*: invalid binding list"

and lambda env params =
  env, Func (build_func env params)

and build_func env params =
  match params with
    | Symbol vararg :: body ->
        {
          params = [];
          vararg = Some vararg;
          body = body;
          closure = env;
        }
    | List params :: body ->
        {
          params = L.map Primitives.unpack_sym params;
          vararg = None;
          body = body;
          closure = env;
        }
    | DottedList (params, Symbol vararg) :: body ->
        {
          params = L.map Primitives.unpack_sym params;
          vararg = Some vararg;
          body = body;
          closure = env;
        }
    | _ -> invalid_arg "lambda: invalid arguments list"

and lazy_prim_macros = lazy (
  [
    "quote", quote;
    "begin", begin_;
    "if", if_;
    "define", define;
    "set!", set;
    "let", let_;
    "letrec", letrec;
    "let*", let_star;
    "lambda", lambda;
  ]
)
;;
