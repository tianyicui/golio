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

let is_symbol arg =
  match unpack_sexp arg with
    | Symbol _ -> bool_ true
    | _ -> bool_ false
;;
let is_number arg =
  match unpack_sexp arg with
    | Number _ -> bool_ true
    | _ -> bool_ false
;;
let is_string arg =
  match unpack_sexp arg with
    | String _ -> bool_ true
    | _ -> bool_ false
;;
let is_bool arg =
  match unpack_sexp arg with
    | Bool _ -> bool_ true
    | _ -> bool_ false
;;
let is_list arg =
  match unpack_sexp arg with
    | List _ -> bool_ true
    | _ -> bool_ false
;;
let is_pair arg =
  match unpack_sexp arg with
    | List _ | DottedList _ -> bool_ true
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

let apply (eval : Type.env -> Type.value -> Type.env * Type.value) id arg_list =
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
          in snd
               (L.fold_left
                  (fun (env', _) sexp -> eval env' sexp)
                  (env, Undefined)
                  func.body)
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

      "symbol->string", string_to_symbol;
      "string->symbol", symbol_to_string;

      "car", unary_op car;
      "cdr", unary_op cdr;
      "cons", binary_op cons;
      "list", list_func;

      "eqv?", bool_any_binop eqv;
      "eq?", bool_any_binop eq;
      "equal?", bool_any_binop equal;

      "apply", binary_op (apply eval);
    ]
;;
