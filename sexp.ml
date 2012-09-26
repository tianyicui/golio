open Type
open Printf

module S = String
module L = List

let rec print_sexp sexp =
  match sexp with
    | Number num -> sprintf "%i" num
    | Symbol str -> sprintf "%s" str
    | String str -> sprintf "%S" str
    | Bool true -> sprintf "#t"
    | Bool false -> sprintf "#f"
    | List [Symbol "quote"; p] ->
        "'" ^ print_sexp p
    | List lst ->
        "(" ^ S.concat " " (L.map print_sexp lst) ^ ")"
    | DottedList (lst, cdr) ->
        "(" ^ S.concat " " (L.map print_sexp lst) ^ " . " ^
        (print_sexp cdr) ^ ")"

and print_func func =
  match func with
    | PrimFunc (name, _) ->
        sprintf "#<procedure:%s>" name
    | UserFunc func ->
        "(lambda " ^
        begin match func.params, func.vararg with
          | [], None -> "()"
          | [], Some arg -> arg
          | params, vararg ->
              "(" ^ S.concat " " params ^
              (match vararg with
                 | None -> ""
                 | Some arg -> " . " ^ arg
              ) ^ ")"
        end ^
        " " ^
        S.concat " " (L.map print_sexp func.body) ^
        ")"
;;

let print_macro macro =
  match macro with
    | PrimMacro (name, _) ->
        sprintf "#<macro:%s>" name
;;

let print_value value =
  match value with
    | Sexp sexp -> print_sexp sexp
    | Func func -> print_func func
    | Macro macro -> print_macro macro
    | Undefined ->
        failwith "print_sexp: should not print Undefined"
;;
