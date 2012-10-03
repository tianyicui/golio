open Type

val map :
  env -> sexp list -> env * value list
;;

val eval_all :
  env -> sexp list -> env * value
;;

val eval :
  ?tail:bool -> env -> sexp -> env * value
;;
