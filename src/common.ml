open Type

module M = Map.Make(String)
module L = List
module H = Hashtbl
module Q = Queue
module S = String

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

let lisp_error error =
  raise (Lisp_error error)
;;
let arg_count_mismatch expected got =
  lisp_error (ArgCountMismatch
                {arg_count_expected = expected;
                 arg_count_got = got})
;;
let arg_type_mismatch expected got =
  lisp_error (ArgTypeMismatch
                {arg_type_expected = expected;
                 arg_type_got = got})
;;
let not_applicable value =
  lisp_error (NotApplicable value)
;;
let unbound_var var =
  lisp_error (UnboundVar var)
;;
let closed_chan chan =
  lisp_error (ClosedChan chan)
;;

let unpack_sexp value =
  match value with
    | Sexp sexp -> sexp
    | _ -> arg_type_mismatch "sexp" value
;;
let unpack_num value =
  match unpack_sexp value with
    | Number num -> num
    | _ -> arg_type_mismatch "number" value
;;
let unpack_sym value =
  match unpack_sexp value with
    | Symbol sym -> sym
    | _ -> arg_type_mismatch "symbol" value
;;
let unpack_str value =
  match unpack_sexp value with
    | String str -> str
    | _ -> arg_type_mismatch "string" value
;;
let unpack_bool value =
  match unpack_sexp value with
    | Bool bl -> bl
    | _ -> arg_type_mismatch "bool" value
;;
let unpack_list value =
  match unpack_sexp value with
    | List lst -> lst
    | _ -> arg_type_mismatch "list" value
;;
let unpack_input_port value =
  match value with
    | Port (InputPort (_, lb, _)) -> lb
    | _ -> arg_type_mismatch "input-port" value
;;
let unpack_input_port_for_channel value =
  match value with
    | Port (InputPort (_, _, channel)) -> channel
    | _ -> arg_type_mismatch "input-port" value
;;
let unpack_output_port value =
  match value with
    | Port (OutputPort (_, channel)) -> channel
    | _ -> arg_type_mismatch "output-port" value
;;
let unpack_chan value =
  match value with
    | Chan chan -> chan
    | _ -> arg_type_mismatch "chan" value
;;
let unpack_promise value =
  match value with
    | Promise promise -> promise
    | _ -> arg_type_mismatch "promise" value
;;

let with_mutex mutex func =
  Mutex.lock mutex;
  let rst = func () in
    Mutex.unlock mutex;
    rst
;;
