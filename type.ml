module M = Map.Make(String)

type value =
  | Sexp of sexp
  | Func of func
  | Macro of macro
  | Undefined

and sexp =
  | Number of int
  | Symbol of string
  | String of string
  | Bool of bool
  | List of value list
  | DottedList of value list * value

and func =
  | PrimFunc of string * (value list -> value)
  | UserFunc of func_record

and macro =
  | PrimMacro of string * (env -> value list -> env * value)

and env =
    value ref M.t

and func_record = {
  params : string list;
  vararg : string option;
  body : sexp list;
  closure : env;
}
;;
