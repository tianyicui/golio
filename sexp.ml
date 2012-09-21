type sexp = Number of int
          | Atom of string
          | String of string
          | Bool of bool
          | List of sexp list
          | DottedList of sexp list * sexp
;;


