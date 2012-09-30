open Type

let thread_queue =
  Q.create ()
;;

let tq_mutex =
  Mutex.create ()
;;

let register_thread thread =
  Mutex.lock tq_mutex;
  Q.push thread thread_queue;
  Mutex.unlock tq_mutex
;;

let init () =
  Mutex.lock tq_mutex;
  Q.clear thread_queue;
  Mutex.unlock tq_mutex;

  Env.clear_globals ()
;;

let finish () =
  Mutex.lock tq_mutex;
  (try
    while true do
      let thread = Q.pop thread_queue in
        Thread.join thread
    done
  with
    | Q.Empty -> ());
  Mutex.unlock tq_mutex
;;
