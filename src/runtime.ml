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
    with_mutex mutex
      (fun () ->
         let id = !n_total in
           n_total := id + 1;
           id)
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
    with_mutex mutex
      (fun () ->
         n_total := !n_total + 1)
  ;;
  let ended () =
    with_mutex mutex
      (fun () ->
         n_total := !n_total - 1;
         check_terminal ();
      )
  ;;
  let blocked () =
    with_mutex mutex
      (fun () ->
         n_blocked := !n_blocked + 1;
         check_terminal ();
      )
  ;;
  let unblocked () =
    with_mutex mutex
      (fun () ->
         n_blocked := !n_blocked - 1)
  ;;
  let create func arg =
    let sync_channel = Event.new_channel () in
    let go () =
      (Event.sync (Event.receive sync_channel);
       (try ignore (func arg) with
          | expn ->
              (* TODO: send backtrace too
              prerr_endline
                (Printf.sprintf "%s in thread #%i:\n%s"
                   (Print.print_exn expn)
                   (Thread.id (Thread.self ()))
                   (Printexc.get_backtrace ()));
               *)
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
  Env.init ()
;;
