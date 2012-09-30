open Type

let quote env param =
  env, (Prim_func.unary_op (fun x -> Sexp x) param)
;;

let begin_ env params =
  Eval.eval_all env params
;;

let if_ env params =
  let pred, conseq, optional_alt =
    match params with
      | [x; y] -> x, y, None
      | [x; y; z] -> x, y, Some z
      | _ -> invalid_arg "if: expected 2 or more arguments"
  in
    match (Eval.eval env pred) with
      | env', Sexp (Bool false) ->
          (match optional_alt with
             | Some alt -> Eval.eval ~tail:true env' alt
             | None -> env', list_ []
          )
      | env', _ -> Eval.eval ~tail:true env' conseq
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

let define env params =
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
          closure = Env.def_local var Undefined func.closure
    } in
    let rst = user_func func' in
      (Env.set_var var rst func'.closure; rst)
  in
  let var, (env', value) =
    match params with
      | [Symbol var; expr] ->
          var, Eval.eval env expr
      | List [] :: _ -> invalid_arg "define: empty definition list"
      | (List (Sexp (Symbol var) :: _) as definition) :: body
      | (DottedList ((Sexp (Symbol var) :: _), _) as definition) :: body ->
          var, (env, named_lambda var (pair_cdr definition :: body))
      | _ -> invalid_arg "define: invalid arguments"
  in
    (if env'.top_level then
       (Env.def_global var value; env')
     else
       Env.def_local var value env'
    ), Undefined
;;

let set env params =
  match params with
    | [Symbol var; expr] ->
        let env', value = Eval.eval env expr in
          (Env.set_var var value env;
           env, Undefined)
    | [_; _] -> invalid_arg "set!: first argument should be a symbol"
    | _ -> invalid_arg "set!: expected 2 arguments"
;;

let let_to_apply is_rec env params =
  let def_vars vars env =
    L.fold_left
      (fun e v ->
         if (Env.is_bound v e)
         then Env.def_local v (Env.get_var v e) e
         else Env.def_local v Undefined e
      )
      env
      vars
  in
  let name = if is_rec then "letrec" else "let" in
    match params with
      | [] -> invalid_arg (name ^ ": should have a binding list")
      | List bindings :: body ->
          let vars, inits = L.split (
            L.map
              (fun binding ->
                 match unpack_sexp binding with
                   | List [Sexp (Symbol var); Sexp init] -> var, init
                   | _ -> invalid_arg (name ^ ": invalid binding list"))
              bindings
          ) in
          let env', values =
            Eval.map (if is_rec then def_vars vars env else env) inits
          in
            begin
              if is_rec then
                L.iter2 (fun var value -> Env.set_var var value env') vars values;
              let func = {params = vars; vararg = None; body = body; closure = env'} in
                env, Prim_func.apply [user_func func; list_ values]
            end
      | _ -> invalid_arg (name ^ ": invalid binding list")
;;

let let_ =
  let_to_apply false
;;

let letrec =
  let_to_apply true
;;

let let_star env params =
  match params with
    | [] -> invalid_arg "let*: should have a binding list"
    | List bindings :: body ->
        let env' =
          L.fold_left
            (fun e b ->
               match b with
                 | Sexp (List l) ->
                     fst (define e (L.map unpack_sexp l))
                 | _ -> invalid_arg "let*: invalid binding list"
            )
            { env with top_level = false }
            bindings
        in
          env, snd (begin_ env' body)
    | _ -> invalid_arg "let*: invalid binding list"
;;

let rec quasiquote level env params =
  let qq_list lst =
    let rec flatten lists =
      let append l r =
        match l with
          | Sexp (List xs) ->
              (match r with
                 | Sexp (List ys) -> List (xs @ ys)
                 | Sexp (DottedList (ys, last)) ->
                     DottedList ((xs @ ys), last)
                 | y ->
                     DottedList (xs, y)
              )
          | _ -> invalid_arg "append: first argument should be a list"
      in
        match lists with
          | [] -> List []
          | [x] -> unpack_sexp x
          | l :: r -> append l (Sexp (flatten r))
    in
      Sexp (flatten (
        L.map
          (fun x ->
             match x with
               | List (Sexp (Symbol "unquote-splicing") :: body) ->
                   let _, rst =
                     unquote
                       ~splicing:true
                       (level - 1)
                       env
                       (L.map unpack_sexp body)
                   in
                     if level = 1 then
                       rst
                     else
                       list_ [rst]
               | _ ->
                   list_ [snd (quasiquote level env [x])]
          )
          lst
      ))
  in
    match params with
      | [List args] ->
          let sexp_list = L.map unpack_sexp args in
            (match sexp_list with
               | Symbol "quasiquote" :: body ->
                   let env', rst =
                     quasiquote (level + 1) env body
                   in
                     env', list_ [symbol "quasiquote"; rst]
               | Symbol "unquote" :: body ->
                   unquote (level - 1) env body
               | _ ->
                   env, qq_list sexp_list
            )
      | [DottedList (args, last)] ->
          let arg_rst =
            unpack_list (qq_list (L.map unpack_sexp args))
          in
          let last_rst =
            snd (quasiquote level env [unpack_sexp last])
          in
            env, dotted_list arg_rst last_rst
      | [arg] -> env, Sexp arg
        | _ -> invalid_arg "quasiquote: should have exactly 1 argument"
and unquote ?(splicing=false) level env params =
  if level != 0 then
    (let env', rst = quasiquote level env params in
     let name = if splicing then "unquote-splicing" else "unquote"
     in
       env', list_ [symbol name; rst])
  else
    match params with
      | [arg] ->
          Eval.eval env arg
      | _ -> invalid_arg "unquote: should have exactly 1 argument"
;;

let lambda env params =
  env, user_func (build_func env params)
;;

let load env params =
  let load_file env filename =
    let in_c = open_in filename in
    let lb = Lexing.from_channel in_c in
    let parse () = Parser.parse Lexer.tokens lb in
    let rec go env =
      match parse () with
        | None -> (close_in in_c; env)
        | Some sexp -> go (fst (Eval.eval env sexp))
    in go env
  in
  match params with
    | [param] ->
        let env', arg = Eval.eval env param in
          (match arg with
             | Sexp (String filename) ->
                 (load_file env' filename, Undefined)
             | _ ->
                 invalid_arg "load: the argument should be a string"
          )
    | _ -> invalid_arg "load: should have one single argument"
;;

let go env param =
  let eval =
     Eval.eval env
  in
  L.iter
    (fun sexp ->
       let thread = Thread.create eval sexp in
         Runtime.register_thread thread)
    param;
  env, Undefined
;;

let prim_macros =
  [
    "quote", quote;
    "begin", begin_;
    "if", if_;
    "define", define;
    "set!", set;
    "let", let_;
    "letrec", letrec;
    "let*", let_star;
    "quasiquote", quasiquote 1;
    "lambda", lambda;
    "load", load;
    "go", go;
  ]
;;
