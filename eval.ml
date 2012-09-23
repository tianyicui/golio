open Types

module L = List
module M = Map.Make(String)

let rec lazy_prim_functions = lazy (
    let open Primitives in
    [
        "+", num_binop (+);
        "-", num_binop (-);
        "*", num_binop ( * );
        "/", num_binop (/);
        "%", num_binop (mod);

        "=", num_bool_binop (==);
        "<", num_bool_binop (<);
        ">", num_bool_binop (>);
        "/=", num_bool_binop (!=);
        ">=", num_bool_binop (>=);
        "<=", num_bool_binop (<=);

        "&&", bool_bool_binop (&&);
        "||", bool_bool_binop (||);

        "string=?", str_bool_binop (=);
        "string<?", str_bool_binop (<);
        "string>?", str_bool_binop (>);
        "string<=?", str_bool_binop (<=);
        "string>=?", str_bool_binop (>=);

        "symbol?", unary_op is_symbol;
        "string?", unary_op is_string;
        "number?", unary_op is_number;
        "bool?", unary_op is_bool;
        "list?", unary_op is_list;

        "symbol->string", string_to_symbol;
        "string->symbol", symbol_to_string;

        "car", unary_op car;
        "cdr", unary_op cdr;
        "cons", binary_op cons;
        "list", list_;

        "eqv?", bool_any_binop eqv;
        "eq?", bool_any_binop eq;
        "equal?", bool_any_binop equal;

        "apply", binary_op apply;
    ]
)

and quote env param =
    env, (Primitives.unary_op (fun x -> x) param)

and begin_ env params =
    match params with
    | [] -> env, Undefined
    | _ ->
        let env', lst = eval_list env params in
        env', L.hd (L.rev lst)

and if_ env params =
    match params with
    | [pred; conseq; alt] -> (
            match (eval env pred) with
            | env', Bool false -> eval env' alt
            | env', _ -> eval env' conseq
            )
    | _ -> invalid_arg "if: expected 3 arguments"

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

and let_ env params =
    match params with
    | [] -> invalid_arg "let: cannot have an empty body"
    | (List bindings_sexp) :: body ->
        let vars, inits = L.split (L.map
            (fun sexp ->
                match sexp with
                | List [Symbol var; init] -> var, init
                | _ -> invalid_arg "let: invalid binding list")
            bindings_sexp)
        in
        let env', values = eval_list env inits in
        let func = {params = vars; vararg = None; body = body; closure = env'} in
        env', apply (Func func) (List values)
    | _ -> invalid_arg "let: invalid binding list"

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

and lazy_prim_macros = lazy
    [
        "quote", quote;
        "begin", begin_;
        "if", if_;
        "define", define;
        "set!", set;
        "let", let_;
        "lambda", lambda;
    ]

and lazy_primitive_env = lazy (
    let env_from_assoc_list f init lst=
        Env.bind_vars init (L.map (fun (k, v) -> (k, f k v)) lst)
    in
    let env_with_func =
        env_from_assoc_list
            (fun k v -> PrimitiveFunc (k, v))
            Env.empty
            (Lazy.force lazy_prim_functions)
    in
    let env =
        env_from_assoc_list
            (fun k v -> PrimitiveMacro (k, v))
            env_with_func
            (Lazy.force lazy_prim_macros)
    in env
)

and eval_list env sexp_list =
    let env, rst_lst = L.fold_left
        (fun accu sexp ->
            let env', lst = accu in
            let env'',rst = eval env' sexp in
            env'', (rst :: lst))
        (env, [])
        sexp_list
    in env, (L.rev rst_lst)

and apply id arg_list =
    let args = (Primitives.unpack_list arg_list) in
    match id with
    | PrimitiveFunc (_, func) ->
        func args
    | Func func ->
        let params_len = L.length func.params in
        let args_len = L.length args in
        let env =
            begin match func.vararg with
            | None ->
                if params_len == args_len then
                    Env.bind_vars func.closure (List.combine func.params args)
                else invalid_arg ("apply: invalid number of args, expected " ^
                    string_of_int params_len ^ ", given " ^ string_of_int args_len)
            | Some vararg ->
                if params_len <= args_len then
                    let rec go params args =
                        begin match params with
                        | [] -> Env.def_var vararg (List args) func.closure
                        | x :: xs -> Env.def_var x (L.hd args) (go xs (L.tl args))
                        end
                    in go func.params args
                else invalid_arg ("apply: invalid number of args, expected " ^
                    string_of_int params_len ^ "+, given " ^ string_of_int args_len)
            end
        in
        snd (begin_ env func.body)
    | _ -> invalid_arg "apply: not applicable"

and eval env sexp =
    match sexp with
    | String _ | Number _ | Bool _
    | PrimitiveFunc _ | Func _
    | PrimitiveMacro _ -> (env, sexp)
    | Symbol id -> (env, Env.get_var id env)
    | List (hd :: tl) ->
            let env', id = eval env hd in
            begin match id with
            | PrimitiveFunc _
            | Func _ ->
                let env'', args = eval_list env' tl in
                env'', (apply id (List args))
            | PrimitiveMacro (_, macro) ->
                macro env' tl
            | _ -> invalid_arg "eval: invalid application"
            end
    | List [] -> invalid_arg "eval: invalid application"
    | DottedList _ -> invalid_arg "eval: cannot eval dotted list"
    | Undefined -> invalid_arg "eval: cannot eval undefined value"
;;
