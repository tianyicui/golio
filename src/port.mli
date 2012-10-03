open Type

val prim_ports :
  unit -> (string * value) list
;;

val init :
  Lexing.lexbuf -> in_channel -> out_channel -> unit
;;
