open Type

val create :
  int -> chan
;;

val send :
  chan -> value -> unit
;;

val receive :
  chan -> value
;;

val close :
  chan -> unit
;;
