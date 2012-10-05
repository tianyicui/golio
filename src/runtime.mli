(** Various runtime support utilities *)

(** Initialize the runtime environment, all the globals and counters will be
* reset. *)
val init :
  unit -> unit
;;

(** Temp file creation *)
module Temp_file :
sig
  (** create a temp file and return its path *)
  val gen :
    unit -> string
  ;;
end

(** {!Type.chan} id generation *)
module Chan_id :
sig
  (** generate a unique id for {!Type.chan} *)
  val gen :
    unit -> int
  ;;
end

(** Runtime configuration for {!.Eval} *)
module Eval :
sig
  val tco :
    bool ref
  ;;
  (** indicates whether tail-call optimization is enabled, used for testing *)
end

(** Runtime globals for {!.Repl} *)
module Repl :
sig
  val exn_channel :
    exn Event.channel
  ;;
  (** [fiber]s will catch their exceptions and send them to this channel for
   * {!Repl.repl} to receive *)
end

(** [fiber]s are the light-weight threads created by [go] macro *)
module Fiber :
sig
  val create :
    ('a -> 'b) -> 'a -> unit
  ;;
  (** create and run a new [fiber] with given function and its argument *)
  val blocked :
    unit -> unit
  ;;
  (** notify the deadlock detector a [fiber] is getting blocked *)
  val unblocked :
    unit -> unit
  ;;
  (** notify the deadlock detector a [fiber] is getting unblocked *)
end
