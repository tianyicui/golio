open Type

(* TODO abstract out? *)
val load :
  env -> sexp list -> env * value
;;

val prim_macros :
  (string * (env -> sexp list -> env * value)) list
;;
