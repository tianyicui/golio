open Type

(* TODO: refactor this type *)
val apply :
  value list -> value
;;

val prim_functions :
  (string * (value list -> value)) list
;;
