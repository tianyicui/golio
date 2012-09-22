open Sexp

module L = List
module M = Map.Make(String)

let primitives =
    let open Primitives in
    L.fold_left
        (fun m (k, v) -> M.add k v m)
        M.empty
        [
            "+", numBinop (+);
            "-", numBinop (-);
            "*", numBinop ( * );
            "/", numBinop (/);
            "%", numBinop (mod);

            "=", numBoolBinop (==);
            "<", numBoolBinop (<);
            ">", numBoolBinop (>);
            "/=", numBoolBinop (!=);
            ">=", numBoolBinop (>=);
            "<=", numBoolBinop (<=);

            "&&", boolBoolBinop (&&);
            "||", boolBoolBinop (||);

            "string=?", strBoolBinop (=);
            "string<?", strBoolBinop (<);
            "string>?", strBoolBinop (>);
            "string<=?", strBoolBinop (<=);
            "string>=?", strBoolBinop (>=);

            "symbol?", unaryOp symbolp;
            "string?", unaryOp stringp;
            "number?", unaryOp numberp;
            "bool?", unaryOp boolp;
            "list?", unaryOp listp;

            "symbol->string", (fun p -> String (unaryOp unpackSym p));
            "string->symbol", (fun p -> Atom (unaryOp unpackStr p));

            "car", unaryOp car;
            "cdr", unaryOp cdr;
            "cons", binaryOp cons;

            "eqv?", boolAnyBinop eqv;
            "eq?", boolAnyBinop  eq;
            "equal?", boolAnyBinop equal;
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
            "quote", Primitives.unaryOp (fun x -> x);
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
