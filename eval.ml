open Types

module L = List
module M = Map.Make(String)

let map_from_assoc_list lst =
    L.fold_left (fun m (k, v) -> M.add k v m) M.empty lst
;;

let primitives =
    let open Primitives in
    map_from_assoc_list [
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

        "eqv?", bool_any_binop eqv;
        "eq?", bool_any_binop eq;
        "equal?", bool_any_binop equal;
    ]
;;

let apply func args map =
    (M.find func map) args
;;
let expand env macro args map =
    (M.find macro map) env args
;;

let rec quote env param =
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
    match params with
    | [Symbol var; expr] ->
        let env', value = eval env expr in
        Env.def_var var value env, Undefined
    | [_; _] -> invalid_arg "define: first argument should be a symbol"
    | _ -> invalid_arg "define: expected 2 arguments"

and set env params =
    match params with
    | [Symbol var; expr] ->
        let env', value = eval env expr in
        Env.set_var var value env, Undefined
    | [_; _] -> invalid_arg "set: first argument should be a symbol"
    | _ -> invalid_arg "set: expected 2 arguments"

and lazy_macros = lazy (
    map_from_assoc_list [
        "quote", quote;
        "begin", begin_;
        "if", if_;
        "define", define;
        "set!", set;
    ])

and eval_list env sexp_list =
    let env, rst_lst = L.fold_left
        (fun accu sexp ->
            let env', lst = accu in
            let env'',rst = eval env' sexp in
            env'', (rst :: lst))
        (env, [])
        sexp_list
    in env, (L.rev rst_lst)
and eval env sexp =
    match sexp with
    | String _ | Number _ | Bool _ -> (env, sexp)
    | Symbol id -> (env, Env.get_var id env)
    | List (Symbol id :: args) ->
            let macros = Lazy.force lazy_macros in
            begin try expand env id args macros
            with Not_found ->
                let env', params = eval_list env args in
                (env', apply id params primitives)
            end
    | List _ -> invalid_arg "eval: invalid application"
    | DottedList _ -> invalid_arg "eval: cannot eval dotted list"
    | Undefined -> invalid_arg "eval: cannot eval undefined value"
;;
