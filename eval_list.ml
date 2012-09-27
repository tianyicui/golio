module L = List

let eval_list eval env value_list =
  let env, rst_lst = L.fold_left
                       (fun (env', lst) value ->
                          let env'',rst = eval env' value in
                            env'', (rst :: lst))
                       (env, [])
                       value_list
  in env, (L.rev rst_lst)
;;
