module L = List

let eval_list eval env sexp_list =
  let env, rst_lst =
    L.fold_left
      (fun (env', lst) sexp ->
         let env'',rst = eval env' sexp in
           env'', (rst :: lst))
      (env, [])
      sexp_list
  in env, (L.rev rst_lst)
;;
