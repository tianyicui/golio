open Type

val init :
  unit -> unit
;;

module Temp_file :
sig
  val gen :
    unit -> string
  ;;
end

module Eval :
sig
  val tco :
    bool ref
  ;;
end

module Chan_id :
sig
  val gen :
    unit -> int
  ;;
end

module Repl :
sig
  val exn_channel :
    exn Event.channel
  ;;
end

module Fiber :
sig
  val create :
    ('a -> 'b) -> 'a -> unit
  ;;
  val blocked :
    unit -> unit
  ;;
  val unblocked :
    unit -> unit
  ;;
end
