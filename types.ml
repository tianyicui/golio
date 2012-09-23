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
