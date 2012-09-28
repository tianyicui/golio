open Type

module L = List

let map eval env sexp_list =
  let env, rst_lst =
    L.fold_left
      (fun (env', lst) sexp ->
         let env'',rst = eval env' sexp in
           env'', (rst :: lst))
      (env, [])
      sexp_list
  in env, (L.rev rst_lst)
;;

let last eval env sexp_list =
  L.fold_left
    (fun (env', _) sexp -> eval env' sexp)
    (env, Undefined)
    sexp_list
;;
