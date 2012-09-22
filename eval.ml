open Sexp

module L = List
module M = Map.Make(String)

let primitives =
    let open Primitives in
    L.fold_left
        (fun m (k, v) -> M.add k v m)
        M.empty
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

            "eqv?", bool_any_binop eqv;
            "eq?", bool_any_binop  eq;
            "equal?", bool_any_binop equal;
        ]
;;

let apply func args map =
    (M.find func map) args
;;

let rec if_ params =
    match params with
    | [pred; conseq; alt] -> (
            match (eval pred) with
            | Bool false -> eval alt
            | _ -> eval conseq
            )
    | _ -> invalid_arg "if: expected 3 arguments"
and macros = lazy (
    L.fold_left
        (fun m (k, v) -> M.add k v m)
        M.empty
        [
            "quote", Primitives.unary_op (fun x -> x);
            "if", if_;
        ]
    )
and eval sexp =
    match sexp with
    | String _ | Number _ | Bool _ -> sexp
    | List (Atom func :: args) ->
            begin try apply func args (Lazy.force macros)
            with Not_found ->
                apply func (L.map eval args) primitives
            end
    | List _ -> invalid_arg "eval: invalid application"
    | DottedList _ -> invalid_arg "eval: cannot eval dotted list"
    | Atom _ -> failwith "not implemented"
;;
