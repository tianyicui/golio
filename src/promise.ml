open Type

let create func =
  Lazy.from_fun func
;;

let is_val promise =
  Lazy.is_val promise
;;

let force promise =
  Lazy.force promise
;;
