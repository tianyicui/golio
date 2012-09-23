open Types

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
