open Type

module L = List

let binary_op op params =
  match params with
    | [x; y] -> op x y
    | _ -> invalid_arg "binary_op: expected exactly 2 arguments"
;;
let num_binop op params =
  match (L.map unpack_num params) with
    | hd :: tl -> number (L.fold_left op hd tl)
    | _ -> invalid_arg "num_binop: expected at least 1 argument"
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
    | xs -> invalid_arg (Printf.sprintf "unary_op: expected 1 argument, given %i"
                           (L.length xs))
;;
let str_unary_op op params =
  match params with
    | [x] -> op (unpack_str x)
    | _ -> invalid_arg "str_unary_op: expected 1 argument"
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

let string_to_symbol arg =
  string_ (unary_op unpack_sym arg)
;;
let symbol_to_string arg =
  symbol (unary_op unpack_str arg)
;;

let car param =
  match unpack_sexp param with
    | List (x :: _) -> x
    | DottedList ((x :: _), _) -> x
    | _ -> invalid_arg "car: expected a pair"
;;
let cdr param =
  match unpack_sexp param with
    | List (_ :: xs) -> list_ xs
    | DottedList ([_], x) -> x
    | DottedList ((_ :: xs), x) -> dotted_list xs x
    | _ -> invalid_arg "cdr: expected a pair"
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
         | [] -> !Port.stdin
         | [port] -> port
         | _ -> invalid_arg "read: expected 0 or 1 arguments")
  in
    match Parser.parse Lexer.tokens lb with
      | Some value -> Sexp value
      | None -> EofObject
;;
let write params =
  match params with
    | [] -> invalid_arg "write: expected 1 or 2 arguments"
    | obj :: remaining ->
        let out_c =
          unpack_output_port
            (match remaining with
               | [] -> !Port.stdout
               | [port] -> port
               | _ -> invalid_arg "write: expected 1 or 2 arguments"
            )
        in
          output_string out_c (Print.print_value obj);
          Undefined
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
  Undefined
;;
let close_output_port arg =
  close_out (unpack_output_port arg);
  Undefined
;;

let apply eval id arg_list =
  let args = (unpack_list arg_list) in
    match (unpack_func id) with
      | PrimFunc (_, func) ->
          func args
      | UserFunc func ->
          let params_len = L.length func.params in
          let args_len = L.length args in
          let env =
            (match func.vararg with
               | None ->
                   if params_len == args_len then
                     Env.bind_vars func.closure (List.combine func.params args)
                   else invalid_arg ("apply: invalid number of args, expected " ^
                                     string_of_int params_len ^ ", given " ^
                                     string_of_int args_len)
               | Some vararg ->
                   if params_len <= args_len then
                     let rec go params args =
                       begin match params with
                         | [] -> Env.def_var vararg (list_ args) func.closure
                         | x :: xs -> Env.def_var x (L.hd args) (go xs (L.tl args))
                       end
                     in go func.params args
                     else invalid_arg ("apply: invalid number of args, expected " ^
                                       string_of_int params_len ^ "+, given " ^
                                       string_of_int args_len)
            )
          in
            snd (Eval_list.last eval env func.body)
;;

let prim_functions eval =
    [
      "+", num_binop (+);
      "-", num_binop (-);
      "*", num_binop ( * );
      "/", num_binop (/);
      "%", num_binop (mod);

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

      "symbol->string", string_to_symbol;
      "string->symbol", symbol_to_string;

      "car", unary_op car;
      "cdr", unary_op cdr;
      "cons", binary_op cons;
      "list", list_func;

      "eqv?", bool_any_binop eqv;
      "eq?", bool_any_binop eq;
      "equal?", bool_any_binop equal;

      "read", read;
      "write", write;
      "open-input-file", str_unary_op open_input_file;
      "open-output-file", str_unary_op open_output_file;
      "close-input-port", unary_op close_input_port;
      "close-output-port", unary_op close_output_port;

      "apply", binary_op (apply eval);
    ]
;;
