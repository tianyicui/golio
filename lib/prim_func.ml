open Type

let binary_op op params =
  match params with
    | [x; y] -> op x y
    | _ -> arg_count_mismatch "2" (L.length params)
;;
let num_binop op init params =
  number (
    match (L.map unpack_num params) with
      | [x] -> op init x
      | hd :: tl -> L.fold_left op hd tl
      | _ -> arg_count_mismatch "1+" (L.length params)
  )
;;
let num_fold_op op init params =
  number (L.fold_left op init (L.map unpack_num params))
;;
let bool_any_binop op args =
  bool_ (binary_op op args)
;;
let bool_binop unpack op args =
  bool_any_binop op (L.map unpack args)
;;
let num_bool_binop =
  bool_binop unpack_num
;;
let bool_bool_binop =
  bool_binop unpack_bool
;;
let str_bool_binop =
  bool_binop unpack_str
;;
let unary_op op params =
  match params with
    | [x] -> op x
    | _ -> arg_count_mismatch "1" (L.length params)
;;
let str_unary_op op params =
  match params with
    | [x] -> op (unpack_str x)
    | _ -> arg_count_mismatch "1" (L.length params)
;;

let is_symbol arg =
  match arg with
    | Sexp (Symbol _) -> bool_ true
    | _ -> bool_ false
;;
let is_number arg =
  match arg with
    | Sexp (Number _) -> bool_ true
    | _ -> bool_ false
;;
let is_string arg =
  match arg with
    | Sexp (String _) -> bool_ true
    | _ -> bool_ false
;;
let is_bool arg =
  match arg with
    | Sexp (Bool _) -> bool_ true
    | _ -> bool_ false
;;
let is_list arg =
  match arg with
    | Sexp (List _) -> bool_ true
    | _ -> bool_ false
;;
let is_pair arg =
  match arg with
    | Sexp (List (_ :: _))
    | Sexp (DottedList _) -> bool_ true
    | _ -> bool_ false
;;
let is_procedure arg =
  match arg with
    | Func _ -> bool_ true
    | _ -> bool_ false
;;
let is_port arg =
  match arg with
    | Port _ -> bool_ true
    | _ -> bool_ false
;;
let is_input_port arg =
  match arg with
    | Port (InputPort _) -> bool_ true
    | _ -> bool_ false
;;
let is_output_port arg =
  match arg with
    | Port (OutputPort _) -> bool_ true
    | _ -> bool_ false
;;
let is_eof_object arg =
  match arg with
    | EofObject -> bool_ true
    | _ -> bool_ false
;;

let symbol_to_string arg =
  string_ (unary_op unpack_sym arg)
;;
let string_to_symbol arg =
  symbol (unary_op unpack_str arg)
;;

let car param =
  match unpack_sexp param with
    | List (x :: _) -> x
    | DottedList ((x :: _), _) -> x
    | _ -> arg_type_mismatch "pair" param
;;
let cdr param =
  match unpack_sexp param with
    | List (_ :: xs) -> list_ xs
    | DottedList ([_], x) -> x
    | DottedList ((_ :: xs), x) -> dotted_list xs x
    | _ -> arg_type_mismatch "pair" param
;;
let cons hd tl =
  match hd, tl with
    | _, Sexp (List xs) -> list_ (hd :: xs)
    | _, Sexp (DottedList (xs, x)) -> dotted_list (hd :: xs) x
    | _ -> dotted_list [hd] tl
;;
let list_func param =
  list_ param
;;

let force value =
  Promise.force (unpack_promise value)
;;

let eqv a b =
  match a, b with
    | Sexp x, Sexp y ->
        (match x, y with
           | List [], List [] -> true
           | List l1, List l2 -> l1 == l2
           | DottedList (l1, t1), DottedList (l2, t2)
             -> (l1 == l2) && (t1 == t2)
           | _ -> x = y
        )
    | _ -> a == b
;;
let eq a b =
  eqv a b
;;
let equal a b =
  a = b
;;

let read params =
  let lb =
    unpack_input_port
      (match params with
         | [] -> Env.get_global "stdin"
         | [port] -> port
         | _ -> arg_count_mismatch "0 or 1" (L.length params))
  in
    match Parser.parse Lexer.tokens lb with
      | Some value -> Sexp value
      | None -> EofObject
;;
let write params =
  match params with
    | [] -> arg_count_mismatch "1 or 2" 0
    | obj :: remaining ->
        let out_c =
          unpack_output_port
            (match remaining with
               | [] -> Env.get_global "stdout"
               | [port] -> port
               | _ -> arg_count_mismatch "1 or 2" (L.length params)
            )
        in
          output_string out_c (Print.print_value obj);
          flush out_c;
          Void
;;
let newline params =
  let out_c =
    unpack_output_port
      (match params with
         | [] -> Env.get_global "stdout"
         | [obj] -> obj
         | _ -> arg_count_mismatch "0 or 1" (L.length params)
      )
  in
    output_string out_c "\n";
    flush out_c;
    Void
;;
let open_input_file file =
  let in_c = open_in file in
    input_port file (Lexing.from_channel in_c) in_c;
;;
let open_output_file file =
  output_port file (open_out file)
;;
let close_input_port arg =
  close_in (unpack_input_port_for_channel arg);
  Void
;;
let close_output_port arg =
  close_out (unpack_output_port arg);
  Void
;;

let apply params =
  match params with
    | []
    | [_] ->
        arg_count_mismatch "2+" 1
    | (Func func) :: args ->
        let arg =
          (let rec go args =
             match args with
               | [last] -> unpack_list last
               | x :: xs -> x :: (go xs)
               | _ -> failwith "unreachable"
           in go args)
        in
          (match func with
            | PrimFunc (_, func) ->
                func arg
            | UserFunc func ->
                Thunk (func, arg))
    | x :: _ -> arg_type_mismatch "procedure" x
;;

let sleep value =
  let time_in_ms =
    (float_of_int (unpack_num value)) /. 1000.0
  in
    Thread.delay time_in_ms;
    Void
;;

let yield params =
  match params with
    | [] -> Thread.yield (); Void
    | _ -> arg_count_mismatch "0" (L.length params)
;;
let make_chan params =
  let cap =
    match params with
      | [] -> 0
      | [num] -> unpack_num num
      | _ -> arg_count_mismatch "0 or 1" (L.length params)
  in
    Chan (Chan.create cap)
;;
let receive chan =
  Chan.receive (unpack_chan chan)
;;
let send chan value =
  Chan.send (unpack_chan chan) value;
  Void
;;
let close_chan chan =
  Chan.close (unpack_chan chan);
  Void
;;

let prim_functions =
    [
      "+", num_fold_op (+) 0;
      "-", num_binop (-) 0;
      "*", num_fold_op ( * ) 1;
      "/", num_binop (/) 1;
      "%", num_binop (mod) 0;

      "=", num_bool_binop (==);
      "<", num_bool_binop (<);
      ">", num_bool_binop (>);
      "/=", num_bool_binop (!=);
      ">=", num_bool_binop (>=);
      "<=", num_bool_binop (<=);

      "&&", bool_bool_binop (&&);
      "||", bool_bool_binop (||);

      "string=?", str_bool_binop (=);
      "string<?", str_bool_binop (<);
      "string>?", str_bool_binop (>);
      "string<=?", str_bool_binop (<=);
      "string>=?", str_bool_binop (>=);

      "symbol?", unary_op is_symbol;
      "string?", unary_op is_string;
      "number?", unary_op is_number;
      "bool?", unary_op is_bool;
      "list?", unary_op is_list;
      "pair?", unary_op is_pair;
      "procedure?", unary_op is_procedure;
      "port?", unary_op is_port;
      "input-port?", unary_op is_input_port;
      "output-port?", unary_op is_output_port;
      "eof-object?", unary_op is_eof_object;

      "symbol->string", symbol_to_string;
      "string->symbol", string_to_symbol;

      "car", unary_op car;
      "cdr", unary_op cdr;
      "cons", binary_op cons;
      "list", list_func;

      "force", unary_op force;

      "eqv?", bool_any_binop eqv;
      "eq?", bool_any_binop eq;
      "equal?", bool_any_binop equal;

      "read", read;
      "write", write;
      "newline", newline;
      "open-input-file", str_unary_op open_input_file;
      "open-output-file", str_unary_op open_output_file;
      "close-input-port", unary_op close_input_port;
      "close-output-port", unary_op close_output_port;

      "apply", apply;

      "yield", yield;
      "sleep", unary_op sleep;
      "make-chan", make_chan;
      "receive", unary_op receive;
      "send", binary_op send;
      "close-chan", unary_op close_chan;
    ]
;;
