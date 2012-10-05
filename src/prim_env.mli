(** The primitive environment *)

open Type

(** create the primitive {!Type.env}. It will load the primitives in
  * {!Prim_func}, {!Prim_macro} and {!Prim_port} as global variables. Then it
  * will evaluate {!Stdlib.stdlib}. *)
val prim_env :
  unit -> env
;;
