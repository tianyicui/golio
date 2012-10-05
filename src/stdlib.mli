(** The standard library *)

(** Goal of the stdlib is as much [R5RS]-compatible as possible, with
  * concurrency support. It is a [string] instead of a separate [.scm] file for
  * simplified building and packaging *)
val stdlib :
  string
;;
