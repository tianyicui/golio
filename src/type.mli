type value =
  | Sexp of sexp
  | Func of func
  | Macro of macro
  | Port of port
  | Chan of chan
  | Promise of promise
  | EofObject
  | Void
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
and promise =
  value Lazy.t
and chan = {
  id: int;
  channel: value Event.channel;
  mutable closed_flag: bool;
  (* When clients_count > 0, its value represents the number of senders
   * blocking on this channel, when clitns_count < 0, its negation represents
   * the number of receivers blocking on this channel. *)
  mutable clients_count: int;
  mutex: Mutex.t;
}
and env = {
  top_level : bool;
  locals: value ref Map.Make(String).t;
}
and user_func = {
  params : string list;
  vararg : string option;
  body : sexp list;
  closure : env;
}
;;

type lisp_error =
  | ParseError (* TODO *)
  | ArgCountMismatch of arg_count_mismatch
  | ArgTypeMismatch of arg_type_mismatch
  | NotApplicable of value
  | UnboundVar of string
  | ClosedChan of chan
and arg_count_mismatch = {
  (* arg_count_expected could be like "0", "1", "2+" "1 or 2" *)
  arg_count_expected : string;
  arg_count_got : int;
}
and arg_type_mismatch = {
  arg_type_expected : string;
  arg_type_got : value;
}
;;

exception Lisp_error of lisp_error
exception Dead_lock
exception Normal_exit
exception Repl_exn of exn list
;;

type repl_config = {
  interactive : bool;
  print_result : bool;
  print_exn : bool;
  lexbuf : Lexing.lexbuf option;
  stdin : in_channel;
  stdout : out_channel;
}
;;
