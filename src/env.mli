open Type

val empty :
  env
;;

val is_bound :
  string -> env -> bool
;;

val get_var :
  string -> env -> value
;;

val get_global :
  string -> value
;;

val def_local :
  string -> value -> env -> env
;;

val def_global :
  string -> value -> unit
;;

val set_var :
  string -> value -> env -> unit
;;

val bind_locals :
  env -> (string * value) list -> env
;;

val bind_globals :
  (string * value) list -> unit
;;

val init :
  unit -> unit
;;
