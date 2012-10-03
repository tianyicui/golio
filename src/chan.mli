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

val unblocked_send :
  chan -> value -> bool
;;

val unblocked_receive :
  chan -> value option
;;

val close :
  chan -> unit
;;
