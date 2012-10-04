val run_str :
  ?print_exn:bool -> string -> string
;;

val test :
  string -> string -> unit
;;

val test_exn :
  string -> exn -> unit
;;

val get_exn :
  (unit -> unit) -> exn
;;
