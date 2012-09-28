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
    | List [Sexp (Symbol "quote"); p] ->
        "'" ^ print_value p
    | List lst ->
        "(" ^ S.concat " " (L.map print_value lst) ^ ")"
    | DottedList (lst, cdr) ->
        "(" ^ S.concat " " (L.map print_value lst) ^ " . " ^
        (print_value cdr) ^ ")"

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

and print_macro macro =
  match macro with
    | PrimMacro (name, _) ->
        sprintf "#<macro:%s>" name

and print_port port =
  match port with
    | InputPort (name, _, _) ->
        sprintf "#<inputport:%s>" name
    | OutputPort (name, _) ->
        sprintf "#<outputport:%s>" name

and print_value value =
  match value with
    | Sexp sexp -> print_sexp sexp
    | Func func -> print_func func
    | Macro macro -> print_macro macro
    | Port port -> print_port port
    | EofObject ->
        "#<eof>"
    | Undefined ->
        failwith "print_value: should not print Undefined"
    | Thunk _ ->
        failwith "print_value: should not print Thunk"
;;
