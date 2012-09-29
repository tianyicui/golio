module M = Map.Make(String)

type value =
  | Sexp of sexp
  | Func of func
  | Macro of macro
  | Port of port
  | EofObject
  | Undefined
  | Thunk of user_func * value list

and sexp =
  | Number of int
  | Symbol of string
  | String of string
  | Bool of bool
  | List of value list
  | DottedList of value list * value

and func =
  | PrimFunc of string * (value list -> value)
  | UserFunc of user_func

and macro =
  | PrimMacro of string * (env -> sexp list -> env * value)

and port =
  | InputPort of string * Lexing.lexbuf * in_channel
  | OutputPort of string * out_channel

and env =
    value ref M.t

and user_func = {
  params : string list;
  vararg : string option;
  body : sexp list;
  closure : env;
}
;;

let number num =
  Sexp (Number num)
;;
let symbol sym =
  Sexp (Symbol sym)
;;
let string_ str =
  Sexp (String str)
;;
let bool_ bl =
  Sexp (Bool bl)
;;
let list_ lst =
  Sexp (List lst)
;;
let dotted_list lst last =
  Sexp (DottedList (lst, last))
;;
let prim_func name func =
  Func (PrimFunc (name, func))
;;
let user_func func =
  Func (UserFunc func)
;;
let input_port name lb channel =
  Port (InputPort (name, lb, channel))
;;
let output_port name channel =
  Port (OutputPort (name, channel))
;;

let unpack_sexp value =
  match value with
    | Sexp sexp -> sexp
    | _ -> invalid_arg "unpack_sexp: expected a sexp"
;;
let unpack_num value =
  match unpack_sexp value with
    | Number num -> num
    | _ -> invalid_arg "unpack_num: expected a number"
;;
let unpack_sym value =
  match unpack_sexp value with
    | Symbol sym -> sym
    | _ -> invalid_arg "unpack_sym: expected a symbol"
;;
let unpack_str value =
  match unpack_sexp value with
    | String str -> str
    | _ -> invalid_arg "unpack_str: expected a string"
;;
let unpack_bool value =
  match unpack_sexp value with
    | Bool bl -> bl
    | _ -> invalid_arg "unpack_bool: expected a bool"
;;
let unpack_list value =
  match unpack_sexp value with
    | List lst -> lst
    | _ -> invalid_arg "unpack_list: expected a list"
;;
let unpack_func value =
  match value with
    | Func func -> func
    | _ -> invalid_arg "unpack_func: expected a func"
;;
let unpack_input_port value =
  match value with
    | Port (InputPort (_, lb, _)) -> lb
    | _ -> invalid_arg "unpack_input_port: expected an input port"
;;
let unpack_input_port_for_channel value =
  match value with
    | Port (InputPort (_, _, channel)) -> channel
    | _ -> invalid_arg "unpack_input_port_for_channel: expected an input port"
;;
let unpack_output_port value =
  match value with
    | Port (OutputPort (_, channel)) -> channel
    | _ -> invalid_arg "unpack_output_port: expected an output port"
;;
