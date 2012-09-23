module M = Map.Make(String)


type sexp =
    | Number of int
    | Symbol of string
    | String of string
    | Bool of bool
    | List of sexp list
    | DottedList of sexp list * sexp
    | PrimitiveFunc of string * (sexp list -> sexp)
    | Func of func
    | PrimitiveMacro of string * (env -> sexp list -> env * sexp)
    | Undefined
and env =
    sexp ref M.t
and func = {
    params : string list;
    vararg : string option;
    body : sexp list;
    closure : env;
}
;;
