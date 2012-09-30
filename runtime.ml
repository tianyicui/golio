open Type

let enable_tco =
  ref true
;;

let chan_id_count =
  ref 0
;;
let chan_id_cnt_mutex =
  Mutex.create ()
;;
let new_chan_id () =
  Mutex.lock chan_id_cnt_mutex;
  let id = !chan_id_count in
    chan_id_count := id + 1;
    Mutex.unlock chan_id_cnt_mutex;
    id
;;

let thread_queue =
  Q.create ()
;;
let tq_mutex =
  Mutex.create ()
;;

let thread_count =
  ref 1
;;
let tc_mutex =
  Mutex.create ()
;;
let blocked_count =
  ref 0
;;
let bc_mutex =
  Mutex.create ()
;;

let check_dead_lock () =
  if !thread_count <= !blocked_count then
    (Mutex.unlock tc_mutex;
     Mutex.unlock bc_mutex;
     raise Dead_lock)
;;

let thread_started thread =
  Mutex.lock tq_mutex;
  Q.push thread thread_queue;
  Mutex.unlock tq_mutex;
  Mutex.lock tc_mutex;
  thread_count := !thread_count + 1;
  Mutex.unlock tc_mutex;
;;
let thread_terminated thread =
  Mutex.lock tc_mutex;
  thread_count := !thread_count - 1;
  check_dead_lock ();
  Mutex.unlock tc_mutex;
;;
let thread_blocked thread =
  Mutex.lock bc_mutex;
  blocked_count := !blocked_count + 1;
  check_dead_lock ();
  Mutex.unlock bc_mutex;
;;
let thread_unblocked thread =
  Mutex.lock bc_mutex;
  blocked_count := !blocked_count - 1;
  Mutex.unlock bc_mutex;
;;

let init () =
  Mutex.lock chan_id_cnt_mutex;
  chan_id_count := 0;
  Mutex.unlock chan_id_cnt_mutex;

  Mutex.lock tq_mutex;
  Q.clear thread_queue;
  Mutex.unlock tq_mutex;

  Mutex.lock tc_mutex;
  thread_count := 1;
  Mutex.unlock tc_mutex;

  Mutex.lock bc_mutex;
  blocked_count := 0;
  Mutex.unlock bc_mutex;

  Env.clear_globals ()
;;
let finish () =
  try
    while true do
      Mutex.lock tq_mutex;
      let thread = Q.pop thread_queue in
        Mutex.unlock tq_mutex;
        Thread.join thread
    done
  with
    | Q.Empty -> Mutex.unlock tq_mutex;
;;
