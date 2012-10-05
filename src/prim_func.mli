(** Primitive functions *)

open Type

val apply :
  value list -> value (* TODO: refactor this type *)
;;

(** association list of all primitive functions. The key of the list is the name
  * of the function. *)
val prim_functions :
  (string * (value list -> value)) list
;;
