module L = List
module M = Map.Make(String)

type sexp =
    | Number of int
    | Symbol of string
    | String of string
    | Bool of bool
    | List of sexp list
    | DottedList of sexp list * sexp
    | Undefined
and env = sexp M.t
;;

let rec print_sexp sexp =
    let open Printf in
    match sexp with
    | Number num -> sprintf "%i" num
    | Symbol str -> sprintf "%s" str
    | String str -> sprintf "%S" str
    | Bool true -> sprintf "#t"
    | Bool false -> sprintf "#f"
    | List [Symbol "quote"; p] -> "'" ^ print_sexp p
    | List lst ->
            "(" ^ String.concat " " (List.map print_sexp lst) ^ ")"
    | DottedList (lst, cdr) ->
            "(" ^ String.concat " " (List.map print_sexp lst) ^ " . " ^
                (print_sexp cdr) ^ ")"
    | Undefined -> failwith "print_sexp: should not print Undefined"
;;

let empty_env =
    M.empty
;;
let is_bound =
    M.mem
;;
let get_var var env =
    !(M.find var env)
;;
let def_var var value env =
    M.add var (ref value) env
;;
let set_var var value env =
    if is_bound var env
    then (M.find var env := value; env)
    else failwith "set_var: cannot set undefined variable"
;;
