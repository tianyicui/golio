open Type

module Temp_file =
struct
  let gen () =
    Filename.temp_file "golio-" ""
  ;;
end

module Eval =
struct
  let tco =
    ref true
  ;;
end

module Chan_id =
struct
  let n_total =
    ref 0
  ;;
  let init () =
    n_total := 0
  ;;
  let mutex =
    Mutex.create ()
  ;;
  let gen () =
    Mutex.lock mutex;
    let id = !n_total in
      n_total := id + 1;
      Mutex.unlock mutex;
      id
  ;;
end

module Repl =
struct
  let exn_channel =
    Event.new_channel ()
  ;;
end

module Fiber =
struct
  let n_total =
    ref 0
  ;;
  let n_blocked =
    ref 0
  ;;
  let init () =
    n_total := 0;
    n_blocked := 0
  ;;
  let mutex =
    Mutex.create ()
  ;;
  let check_terminal () =
    assert (!n_total >= !n_blocked);
    if !n_total == !n_blocked then
      Event.sync (Event.send Repl.exn_channel
                    (if (!n_total == 0) then Normal_exit else Dead_lock))
  ;;
  let started () =
    Mutex.lock mutex;
    n_total := !n_total + 1;
    Mutex.unlock mutex;
  ;;
  let ended () =
    Mutex.lock mutex;
    n_total := !n_total - 1;
    check_terminal ();
    Mutex.unlock mutex;
  ;;
  let blocked () =
    Mutex.lock mutex;
    n_blocked := !n_blocked + 1;
    check_terminal ();
    Mutex.unlock mutex;
  ;;
  let unblocked () =
    Mutex.lock mutex;
    n_blocked := !n_blocked - 1;
    Mutex.unlock mutex;
  ;;
  let create func arg =
    let sync_channel = Event.new_channel () in
    let go () =
      (Event.sync (Event.receive sync_channel);
       (try ignore (func arg) with
          | expn ->
              Event.sync (Event.send Repl.exn_channel expn));
       ended ())
    in
    ignore (Thread.create go ());
    started ();
    Event.sync (Event.send sync_channel ())
  ;;
end

let init () =
  Chan_id.init ();
  Fiber.init ();
  Env.clear_globals ()
;;
