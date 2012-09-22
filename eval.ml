open Sexp

module L = List
module M = Map.Make(String)

let unpackNum sexp =
    match sexp with
    | Number num -> num
    | _ -> invalid_arg "unpackNum: expected a number"
;;
let unpackSym sexp =
    match sexp with
    | Atom sym -> sym
    | _ -> invalid_arg "unpackSym: expected a symbol"
;;
let unpackStr sexp =
    match sexp with
    | String str -> str
    | _ -> invalid_arg "unpackStr: expected a string"
;;
let unpackBool sexp =
    match sexp with
    | Bool bl -> bl
    | _ -> invalid_arg "unpackBool: expected a bool"
;;
let id sexp =
    sexp
;;

let binaryOp op params =
    match params with
    | [x; y] -> op x y
    | _ -> invalid_arg "binaryOp: expected exactly 2 arguments"
;;
let numBinop op params =
    match (L.map unpackNum params) with
    | hd :: tl -> Number (L.fold_left op hd tl)
    | _ -> invalid_arg "numBinop: expected at least 1 argument"
;;
let boolBinop unpack op args =
    Bool (binaryOp op (L.map unpack args))
;;
let numBoolBinop =
    boolBinop unpackNum
;;
let boolBoolBinop =
    boolBinop unpackBool
;;
let strBoolBinop =
    boolBinop unpackStr
;;
let unaryOp op params =
    match params with
    | [x] -> op x
    | xs -> invalid_arg (Printf.sprintf "unaryOp: expected 1 argument, given %i"
                                        (L.length xs))
;;

let symbolp arg =
    match arg with
    | Atom _ -> Bool true
    | _ -> Bool false
;;
let numberp arg =
    match arg with
    | Number _ -> Bool true
    | _ -> Bool false
;;
let stringp arg =
    match arg with
    | String _ -> Bool true
    | _ -> Bool false
;;
let boolp arg =
    match arg with
    | Bool _ -> Bool true
    | _ -> Bool false
;;
let listp arg =
    match arg with
    | List _ | DottedList _ -> Bool true
    | _ -> Bool false
;;

let car param =
    match param with
    | List (x :: _) -> x
    | DottedList ((x :: _), _) -> x
    | _ -> invalid_arg "car: expected a pair"
;;
let cdr param =
    match param with
    | List (_ :: xs) -> List xs
    | DottedList ([_], x) -> x
    | DottedList ((_ :: xs), x) -> DottedList (xs, x)
    | _ -> invalid_arg "cdr: expected a pair"
;;
let cons hd tl =
    match hd, tl with
    | _, List xs -> List (hd :: xs)
    | _, DottedList (xs, x) -> DottedList ((hd :: xs), x)
    | _ -> DottedList ([hd], tl)
;;

let eqv a b =
    match a, b with
    | Bool x, Bool y -> x = y
    | Atom x, Atom y -> x = y
    | Number x, Number y -> x = y
    | List [], List [] -> true
    | _ -> a == b
;;
let eq a b =
    eqv a b
;;
let equal a b =
    a = b
;;

let primitives =
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

            "eqv?", boolBinop id eqv;
            "eq?", boolBinop id eq;
            "equal?", boolBinop id equal;
        ]
;;

let apply func args =
    let prim =
        try M.find func primitives
        with Not_found -> invalid_arg "apply: undefined function"
    in prim args
;;

let rec eval sexp =
    match sexp with
    | String _ | Number _ | Bool _ -> sexp
    | List [Atom "quote"; p] -> p
    | List (Atom func :: args) -> apply func (L.map eval args)
    | List _ -> invalid_arg "eval: invalid application"
    | DottedList _ -> invalid_arg "eval: cannot eval dotted list"
    | Atom _ -> failwith "not implemented"
;;
