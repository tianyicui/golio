open Type

module L = List
module M = Map.Make(String)

let empty =
  M.empty
;;

let is_bound =
  M.mem
;;

let get_var var (env:env) =
  try !(M.find var env) with
    | Not_found -> failwith ("get_var: cannot get undefined variable " ^ var)
;;

let def_var var value (env:env) =
  M.add var (ref value) env
;;

let set_var var value (env:env) =
  if is_bound var env
  then M.find var env := value
  else failwith ("set_var: cannot set undefined variable " ^ var)
;;

let bind_vars init_env var_list =
  L.fold_left (fun m (k, v) -> def_var k v m) init_env var_list
;;
