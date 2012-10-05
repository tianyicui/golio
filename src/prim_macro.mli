(** Primitive macros *)

open Type

val load :
  env -> sexp list -> env * value
;;
(* TODO abstract out, to Eval or something? *)

(** association list of all primitive macros. The key of the list is the name
  * of the macro. *)
val prim_macros :
  (string * (env -> sexp list -> env * value)) list
;;
