open Type

module S = String
module L = List

let rec print_sexp sexp =
  let open Printf in
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
      | PrimitiveFunc (name, _) ->
          sprintf "#<procedure:%s>" name
      | Func func ->
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
      | PrimitiveMacro (name, _) ->
          sprintf "#<macro:%s>" name
      | Undefined ->
          failwith "print_sexp: should not print Undefined"
;;
