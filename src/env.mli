(** The environment consists of local and global variables *)

open Type

(* TODO: should add a `leave_top_level` function *)

val empty :
  env
;;
(** create an empty local {!Type.env} *)

val is_bound :
  string -> env -> bool
;;
(** whether a variable is bound in an {!Type.env} *)

val get_var :
  string -> env -> value
;;
(** get a {!Type.value} from an {!Type.env}, may raise [UnboundVar] *)

val get_global :
  string -> value
;;
(** get a global variable, may raise [Not_found] *)

val def_local :
  string -> value -> env -> env
;;
(** define a local variable, return a new {!Type.env} *)

val def_global :
  string -> value -> unit
;;
(** define a global variable *)

val set_var :
  string -> value -> env -> unit
;;
(** set a variable to a new value, may raise [UnboundVar] *)

val bind_locals :
  env -> (string * value) list -> env
;;
(** define many local variables with a association list *)

val bind_globals :
  (string * value) list -> unit
;;
(** define many global variables with a association list *)

val init :
  unit -> unit
;;
(** clear all the global variables *)
