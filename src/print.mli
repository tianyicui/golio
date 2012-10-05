(** Represent various types as string *)

open Type

(** represent a {!Type.sexp} as [string] *)
val print_sexp :
  sexp -> string
;;

(** represent a {!Type.value} as [string] *)
val print_value :
  value -> string
;;

(** represent an exception as string, when it's not a kind of exception defined
  * in {!Type}, it will use [Printexc.to_string]. *)
val print_exn :
  exn -> string
;;
