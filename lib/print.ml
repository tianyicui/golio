open Type

let sprintf =
  Printf.sprintf
;;

let rec print_sexp sexp =
  match sexp with
    | Number num -> sprintf "%i" num
    | Symbol str -> sprintf "%s" str
    | String str -> sprintf "%S" str
    | Bool true -> sprintf "#t"
    | Bool false -> sprintf "#f"
    | List [Sexp (Symbol "quote"); p] ->
        "'" ^ print_value p
    | List [Sexp (Symbol "quasiquote"); p] ->
        "`" ^ print_value p
    | List [Sexp (Symbol "unquote"); p] ->
        "," ^ print_value p
    | List [Sexp (Symbol "unquote-splicing"); p] ->
        ",@" ^ print_value p
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

and print_chan chan =
  sprintf "#<chan:%i>" chan.id

and print_promise promise =
  if Promise.is_val promise then
    sprintf "#<promise!%s>" (print_value (Promise.force promise))
  else
    "#<promise>"

and print_value value =
  match value with
    | Sexp sexp -> print_sexp sexp
    | Func func -> print_func func
    | Macro macro -> print_macro macro
    | Port port -> print_port port
    | Chan chan -> print_chan chan
    | Promise promise -> print_promise promise
    | EofObject -> "#<eof>"
    | Void -> "#<void>"
    | Thunk _ ->
        failwith "print_value: should not print Thunk"
;;

let rec print_exn expn =
  match expn with
    | Lisp_error error ->
        (match error with
           | ParseError -> "ParseError"
           | ArgCountMismatch {
               arg_count_expected = expected;
               arg_count_got = got;
             } ->
               sprintf "ArgCountMismatch: expected %s, got %i"
                       expected got
           | ArgTypeMismatch {
               arg_type_expected = expected;
               arg_type_got = got
             } ->
               sprintf "ArgTypeMismatch: expected %s, got %s"
                       expected (print_value got)
           | NotApplicable value ->
               sprintf "NotApplicable: %s" (print_value value)
           | UnboundVar var ->
               sprintf "UnboundVar: %s" var
           | ClosedChan chan ->
               sprintf "ClosedChan: %s" (print_chan chan)
        )
    | Dead_lock -> "Dead_lock"
    | Repl_exn lst ->
        "Repl_exn:\n    " ^ S.concat "\n    " (L.map print_exn lst)
    | expn -> Printexc.to_string expn
;;
