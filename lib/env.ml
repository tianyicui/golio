open Type

let globals =
  H.create 1
;;

let glb_mutex =
  Mutex.create ()
;;

let empty =
  {top_level = true; locals = M.empty}
;;

let is_bound var env =
  M.mem var env.locals || H.mem globals var
;;

let get_ref var env =
  try M.find var env.locals with
    | Not_found -> H.find globals var
;;

let get_var var env =
  try !(get_ref var env) with
    | Not_found ->
        unbound_var var
;;

let get_global var =
  !(H.find globals var)
;;

let def_local var value env =
  { env with locals = M.add var (ref value) env.locals}
;;

let def_global var value =
  Mutex.lock glb_mutex;
  H.add globals var (ref value);
  Mutex.unlock glb_mutex
;;

let set_var var value env =
  try get_ref var env := value with
    | Not_found ->
        unbound_var var
;;

let bind_locals env var_assoc =
  L.fold_left
    (fun m (k, v) -> def_local k v m)
    env
    var_assoc
;;

let bind_globals var_assoc =
  L.iter
    (fun (k, v) -> def_global k v)
    var_assoc
;;

let init () =
  Mutex.lock glb_mutex;
  H.clear globals;
  Mutex.unlock glb_mutex
;;
