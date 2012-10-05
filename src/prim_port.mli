(** The primitive input and output ports *)
open Type

(** returns the association list of primitive ports *)
val prim_ports :
  unit -> (string * value) list
;;

(** initialize the primitive ports; the parameters are the program [lexbuf], the
  * [stdin] channel (not necessarily the same one constructing the [lexbuf]), the
  * [stdout] channel. *)
val init :
  Lexing.lexbuf -> in_channel -> out_channel -> unit
;;
