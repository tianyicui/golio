open Type

let thread_queue =
  Q.create ()
;;

let register_thread thread =
  Q.push thread thread_queue
;;

let init () =
  Env.clear_globals ();
  Q.clear thread_queue
;;

let finish () =
  (try
    while true do
      let thread = Q.pop thread_queue in
        Thread.join thread
    done
  with
    | Q.Empty -> ());
;;
