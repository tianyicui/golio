(** Creation, sending, receiving and closing of {!Type.chan} *)

open Type

(** create a new {!Type.chan} *)
val create :
  unit -> chan
;;

(** send a {!Type.value} to a {!Type.chan}, will block *)
val send :
  chan -> value -> unit
;;

(** receive a {!Type.value} from a {!Type.chan} *)
val receive :
  chan -> value
;;

(** try to send a {!Type.value} to a {!Type.chan}, won't block, return if
  * sending is successful *)
val unblocked_send :
  chan -> value -> bool
;;

(** try to receive a {!Type.value} from a {!Type.chan}, won't block, return
  * [None] if nothing can be received *)
val unblocked_receive :
  chan -> value option
;;

(** after a {!Type.chan} is closed, sending to it raises [Closed_chan], and
  * receiving from it returns [EofObject] *)
val close :
  chan -> unit
;;
