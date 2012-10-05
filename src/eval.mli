(** Evaluate {!Type.sexp} to {!Type.value} *)
open Type

val eval :
  ?tail:bool -> env -> sexp -> env * value
;;
(** Given an {!Type.env}, evaluate a {!Type.sexp} in it, returns a new
  * {!Type.env} and the result {!Type.value}. Use the parameter [tail] when the
  * expression is a
  * {{:http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-6.html#%_sec_3.5}
  * tail expression } to enable tail-call optimization *)

val map :
  env -> sexp list -> env * value list
;;
(** map a [list] of {!Type.sexp} to their corresponding {!Type.value}, the
  * {!Type.sexp}s are evaluated from left to right, and the following {!Type.sexp}s
  * will be evaluated in the {!Type.env} modified by the previous {!Type.sexp}s. *)

val eval_all :
  env -> sexp list -> env * value
;;
(** evaluate a [list] of {!Type.sexp}s sequentially, and return the result of
  * last one. *)
