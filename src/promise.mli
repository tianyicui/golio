(** operations on {!Type.promise} *)

open Type

(** create a new {!Type.promise} from a function that will calculate its value *)
val create :
  (unit -> value) -> promise
;;

(** check whthere a {!Type.promise} has already been calculated *)
val is_val :
  promise -> bool
;;

(** get the {!Type.value} of a {!Type.promise}, will not re-calculate if already
  * did *)
val force :
  promise -> value
;;
