type sexp = Number of int
          | Atom of string
          | String of string
          | Bool of bool
          | List of sexp list
          | DottedList of sexp list * sexp
;;

let rec show sexp = let open Printf in
    match sexp with
    | Number num -> sprintf "%i" num
    | Atom str -> sprintf "%s" str
    | String str -> sprintf "\"%S\"" str
    | Bool true -> sprintf "#t"
    | Bool false -> sprintf "#f"
    | List lst ->
            "(" ^ String.concat " " (List.map show lst) ^ ")"
    | DottedList (lst, cdr) ->
            "(" ^ String.concat " " (List.map show lst) ^ " . " ^ (show cdr)
;;
