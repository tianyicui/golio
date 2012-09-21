open Sexp

let unpackNum sexp =
    match sexp with
    | Number num -> num
    | _ -> invalid_arg "unpackNum: expected a number"
;;

let binNumOp op params =
    match (List.map unpackNum params) with
    | hd :: tl -> Number (List.fold_left op hd tl)
    | _ -> invalid_arg "binNumOp: expected at least 1 argument"
;;

let primitives = [
    "+", binNumOp (+);
    "-", binNumOp (-);
    "*", binNumOp ( * );
    "/", binNumOp (/);
    "%", binNumOp (mod);
];;

let apply func args =
    let prim =
        try List.assoc func primitives
        with Not_found -> invalid_arg "apply: undefined function"
    in prim args
;;

let rec eval sexp =
    match sexp with
    | String _ | Number _ | Bool _ -> sexp
    | List [Atom "quote"; p] -> p
    | List (Atom func :: args) -> apply func (List.map eval args)
    | List _ -> invalid_arg "eval: invalid application"
    | DottedList _ -> invalid_arg "eval: cannot eval dotted list"
    | Atom _ -> failwith "not implemented"
;;
