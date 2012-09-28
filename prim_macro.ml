open Type

module L = List

let quote eval env param =
  env, (Prim_func.unary_op (fun x -> Sexp x) param)
;;

let begin_ eval env params =
  Eval_list.last eval env params
;;

let if_ eval env params =
  let pred, conseq, optional_alt =
    match params with
      | [x; y] -> x, y, None
      | [x; y; z] -> x, y, Some z
      | _ -> invalid_arg "if: expected 2 or more arguments"
  in
    match (eval env pred) with
      | env', Sexp (Bool false) ->
          (match optional_alt with
             | Some alt -> eval env' alt
             | None -> env', list_ []
          )
      | env', _ -> eval env' conseq
;;

let build_func env params =
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
          params = L.map unpack_sym params;
          vararg = None;
          body = body;
          closure = env;
        }
    | DottedList (params, Sexp (Symbol vararg)) :: body ->
        {
          params = L.map unpack_sym params;
          vararg = Some vararg;
          body = body;
          closure = env;
        }
    | _ -> invalid_arg "lambda: invalid arguments list"
;;

let define eval env params =
  let pair_cdr sexp =
    match sexp with
      | List (_ :: xs) -> List xs
      | DottedList ([_], x) -> unpack_sexp x
      | DottedList ((_ :: xs), x) -> DottedList (xs, x)
      | _ -> failwith "unreachable"
  in
  let named_lambda var params =
    let func = build_func env params in
    let func' = {
      func with
          closure = Env.def_var var Undefined func.closure
    } in
    let rst = user_func func' in
      (Env.set_var var rst func'.closure; rst)
  in
  let var, (env', value) =
    match params with
      | [Symbol var; expr] ->
          var, eval env expr
      | List [] :: _ -> invalid_arg "define: empty definition list"
      | (List (Sexp (Symbol var) :: _) as def) :: body
      | (DottedList ((Sexp (Symbol var) :: _), _) as def) :: body ->
          var, (env, named_lambda var (pair_cdr def :: body))
      | _ -> invalid_arg "define: invalid arguments"
  in Env.def_var var value env', Undefined
;;

let set eval env params =
  match params with
    | [Symbol var; expr] ->
        let env', value = eval env expr in
          (Env.set_var var value env; env, Undefined)
    | [_; _] -> invalid_arg "set!: first argument should be a symbol"
    | _ -> invalid_arg "set!: expected 2 arguments"
;;

let let_to_apply is_rec eval env params =
  let name = if is_rec then "letrec" else "let" in
  match params with
    | [] -> invalid_arg (name ^ ": should have a binding list")
    | List bindings_list :: body ->
        let bindings =
          L.map
            (fun init ->
               match unpack_sexp init with
                 | List [Sexp (Symbol var); Sexp init] -> var, init
                 | _ -> invalid_arg (name ^ ": invalid binding list"))
            bindings_list
        in
        let vars, inits = L.split bindings in
        let env', values =
          Eval_list.map
            eval
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
            env, Prim_func.apply eval (user_func func) (list_ values)
        end
    | _ -> invalid_arg (name ^ ": invalid binding list")
;;

let let_ =
  let_to_apply false
;;

let letrec =
  let_to_apply true
;;

let let_star eval env params =
  match params with
    | [] -> invalid_arg "let*: should have a binding list"
    | List [] :: body ->
        begin_ eval env body
    | List (first_binding :: remaining_bindings) :: body ->
        let_ eval env [List [first_binding];
                       List (symbol "let*" ::
                             list_ remaining_bindings ::
                             (L.map (fun s -> Sexp s) body))]
    | _ -> invalid_arg "let*: invalid binding list"
;;

let lambda eval env params =
  env, user_func (build_func env params)
;;

let prim_macros eval =
  L.map
    (fun (k, v) -> (k, v eval))
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
;;
