open Sexp

module L = List

let unpack_num sexp =
    match sexp with
    | Number num -> num
    | _ -> invalid_arg "unpack_num: expected a number"
;;
let unpack_sym sexp =
    match sexp with
    | Symbol sym -> sym
    | _ -> invalid_arg "unpack_sym: expected a symbol"
;;
let unpack_str sexp =
    match sexp with
    | String str -> str
    | _ -> invalid_arg "unpack_str: expected a string"
;;
let unpack_bool sexp =
    match sexp with
    | Bool bl -> bl
    | _ -> invalid_arg "unpack_bool: expected a bool"
;;

let binary_op op params =
    match params with
    | [x; y] -> op x y
    | _ -> invalid_arg "binary_op: expected exactly 2 arguments"
;;
let num_binop op params =
    match (L.map unpack_num params) with
    | hd :: tl -> Number (L.fold_left op hd tl)
    | _ -> invalid_arg "num_binop: expected at least 1 argument"
;;
let bool_any_binop op args =
    Bool (binary_op op args)
let bool_binop unpack op args =
    bool_any_binop op (L.map unpack args)
;;
let num_bool_binop =
    bool_binop unpack_num
;;
let bool_bool_binop =
    bool_binop unpack_bool
;;
let str_bool_binop =
    bool_binop unpack_str
;;
let unary_op op params =
    match params with
    | [x] -> op x
    | xs -> invalid_arg (Printf.sprintf "unary_op: expected 1 argument, given %i"
                                        (L.length xs))
;;

let is_symbol arg =
    match arg with
    | Symbol _ -> Bool true
    | _ -> Bool false
;;
let is_number arg =
    match arg with
    | Number _ -> Bool true
    | _ -> Bool false
;;
let is_string arg =
    match arg with
    | String _ -> Bool true
    | _ -> Bool false
;;
let is_bool arg =
    match arg with
    | Bool _ -> Bool true
    | _ -> Bool false
;;
let is_list arg =
    match arg with
    | List _ | DottedList _ -> Bool true
    | _ -> Bool false
;;

let string_to_symbol arg =
    String (unary_op unpack_sym arg)
;;
let symbol_to_string arg =
    Symbol (unary_op unpack_str arg)
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
    | Symbol x, Symbol y -> x = y
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
