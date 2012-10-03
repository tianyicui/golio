open Type

val create :
  (unit -> value) -> promise
;;

val is_val :
  promise -> bool
;;

val force :
  promise -> value
;;
