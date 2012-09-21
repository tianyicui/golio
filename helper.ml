let compose f g = fun x -> f (g x)
;;

let parse_str str =
    let lb = Lexing.from_string str in
    let rec go () =
        match Parser.parse Lexer.tokens lb with
        | None -> []
        | Some x -> x :: go ()
    in go ()
;;

let rec print_sexp sexp =
    let open Printf in
    let open Sexp in
    match sexp with
    | Number num -> sprintf "%i" num
    | Atom str -> sprintf "%s" str
    | String str -> sprintf "%S" str
    | Bool true -> sprintf "#t"
    | Bool false -> sprintf "#f"
    | List [Atom "quote"; p] -> "'" ^ print_sexp p
    | List lst ->
            "(" ^ String.concat " " (List.map print_sexp lst) ^ ")"
    | DottedList (lst, cdr) ->
            "(" ^ String.concat " " (List.map print_sexp lst) ^ " . " ^
                (print_sexp cdr) ^ ")"
;;

let repl_str str =
    String.concat "\n"
        (List.map (compose print_sexp Eval.eval) (parse_str str))
;;
