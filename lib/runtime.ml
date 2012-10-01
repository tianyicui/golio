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

let repl_exn_channel =
  Event.new_channel ()
;;

let thread_queue =
  Q.create ()
;;
let tq_mutex =
  Mutex.create ()
;;

let thread_count =
  ref 0
;;
let blocked_count =
  ref 0
;;
let tc_bc_mutex =
  Mutex.create ()
;;

let check_terminal () =
  assert (!thread_count >= !blocked_count);
  if !thread_count == !blocked_count then
    Event.sync (Event.send repl_exn_channel
       (if (!thread_count == 0) then Normal_exit else Dead_lock))
;;

let thread_started thread =
  Mutex.lock tq_mutex;
  Q.push thread thread_queue;
  Mutex.unlock tq_mutex;
  Mutex.lock tc_bc_mutex;
  thread_count := !thread_count + 1;
  Mutex.unlock tc_bc_mutex;
;;
let thread_ended thread =
  Mutex.lock tc_bc_mutex;
  thread_count := !thread_count - 1;
  check_terminal ();
  Mutex.unlock tc_bc_mutex;
;;
let thread_blocked thread =
  Mutex.lock tc_bc_mutex;
  blocked_count := !blocked_count + 1;
  check_terminal ();
  Mutex.unlock tc_bc_mutex;
;;
let thread_unblocked thread =
  Mutex.lock tc_bc_mutex;
  blocked_count := !blocked_count - 1;
  Mutex.unlock tc_bc_mutex;
;;

let init () =
  Mutex.lock chan_id_cnt_mutex;
  chan_id_count := 0;
  Mutex.unlock chan_id_cnt_mutex;

  Mutex.lock tq_mutex;
  Q.clear thread_queue;
  Mutex.unlock tq_mutex;

  Mutex.lock tc_bc_mutex;
  thread_count := 0;
  blocked_count := 0;
  Mutex.unlock tc_bc_mutex;

  Env.clear_globals ()
;;
let new_thread func arg =
  let sync_channel = Event.new_channel () in
  let go () =
    (Event.sync (Event.receive sync_channel);
     (try ignore (func arg) with
        | expn ->
            Event.sync (Event.send repl_exn_channel expn));
     thread_ended (Thread.self ()))
  in
  let thread = Thread.create go () in
    (thread_started thread;
     Event.sync (Event.send sync_channel ()))
;;
