module M = Map.Make(String)

let empty_env =
    M.empty
;;

let is_bound =
    M.mem
;;

let get_var var env =
    try !(M.find var env) with
    | Not_found -> failwith "get_var: cannot get undefined variable"
;;

let def_var var value env =
    M.add var (ref value) env
;;

let set_var var value env =
    if is_bound var env
    then (M.find var env := value; env)
    else failwith "set_var: cannot set undefined variable"
;;
