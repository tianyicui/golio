(** All types are defined here *)

(** {2 Different kinds of [value] and the runtime [env]} *)

(** In evaluation, [value] is the intermediate and final result *)
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

(** [sexp], i.e., s-expression, is the input of evaluation *)
and sexp =
  | Number of int
  | Symbol of string
  | String of string
  | Bool of bool
  | List of value list
  | DottedList of value list * value

and func =
  | PrimFunc of string * (value list -> value) (* TODO: to a record *)
  | UserFunc of user_func

and macro =
  | PrimMacro of string * (env -> sexp list -> env * value) (** currently only primitive macro is implemented *) (* TODO: to a record *)

and port =
  | InputPort of string * Lexing.lexbuf * in_channel
  (** we need its [lexbuf] for primitive function
    * [read], and its [in_channel] for [close-input-port] *)
  | OutputPort of string * out_channel

and promise =
  value Lazy.t

and chan = {
  id: int; (** unique chan id *) (* TODO: rename to `chan_id`? *)
  channel: value Event.channel;
  mutable closed_flag: bool;
  mutable clients_count: int;
  mutex: Mutex.t;
}
(** When a [chan]'s [clients_count > 0], its value represents the number of
 * senders blocking on this [chan], when [clients_count < 0], its negation
 * represents the number of receivers blocking on this [chan].*)

(** Runtime local environment, including the info whether we are in top level,
* and all the non-global scope variables. *)
and env = {
  top_level : bool;
  (** whether we are in [top_level] will affect [define]'s behavior *)
  locals: value ref Map.Make(String).t;
}

and user_func = {
  params : string list;
  vararg : string option;
  body : sexp list;
  closure : env;
}
;;

(** {2 Exceptions} *)

(** [lisp_error] encodes the possible runtime errors *)
type lisp_error = (* TODO: better name? *)
  | ParseError (* TODO *)
  | ArgCountMismatch of arg_count_mismatch
  | ArgTypeMismatch of arg_type_mismatch
  | NotApplicable of value
  | UnboundVar of string
  | ClosedChan of chan
and arg_count_mismatch = {
  arg_count_expected : string;
  (** could be like "0", "1", "2+" "1 or 2" *)
  arg_count_got : int;
}
and arg_type_mismatch = {
  arg_type_expected : string;
  arg_type_got : value;
}
;;

exception Lisp_error of lisp_error
exception Dead_lock (** raised when all fibers are waiting for [chan]*)
exception Normal_exit (** used for notifying the repl we exited normally *)
exception Repl_exn of exn list (** the repl can throw a single exn containing all the exceptions fibers throwed *)
;;

(** {2 Config for Repl.repl} *)

(** [repl_config] is The argument type of Repl.repl to finely tune its behavior *)
type repl_config = {
  interactive : bool;
  print_result : bool;
  print_exn : bool;
  lexbuf : Lexing.lexbuf option;
  stdin : in_channel;
  stdout : out_channel;
}
;;
