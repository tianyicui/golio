open Sexp

module L = List

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
let boolAnyBinop op args =
    Bool (binaryOp op args)
let boolBinop unpack op args =
    boolAnyBinop op (L.map unpack args)
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
