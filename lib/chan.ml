open Type

let create capacity = {
  id = Runtime.Chan_id.gen ();
  channel = Event.new_channel ();
  capacity = capacity;
  closed_flag = false;
  closed_flag_mutex = Mutex.create ();
  buffer = Q.create ();
  buffer_mutex = Mutex.create ();
  clients_count = 0;
  clients_count_mutex = Mutex.create ();
}
;;

let maintain_clients_count chan delta =
  with_mutex chan.clients_count_mutex
    (fun () ->
       let will_block = (delta * chan.clients_count >= 0) in
         (if will_block then
            (chan.clients_count <- chan.clients_count + delta;
             Runtime.Fiber.blocked ())
          else
            (chan.clients_count <- chan.clients_count - delta;
             Runtime.Fiber.unblocked ()));
    )
;;

let send chan value =
  with_mutex chan.closed_flag_mutex
    (fun () ->
       if chan.closed_flag then closed_chan chan;
    );

  Mutex.lock chan.buffer_mutex;
  if Q.length chan.buffer < chan.capacity then
    (Q.push value chan.buffer;
  Mutex.unlock chan.buffer_mutex)
  else
    (Mutex.unlock chan.buffer_mutex;
     maintain_clients_count chan 1;
     Event.sync (Event.send chan.channel value))
;;

let receive chan =
  Mutex.lock chan.buffer_mutex;
  if not (Q.is_empty chan.buffer) then
    (let rst = Q.pop chan.buffer in
       Mutex.unlock chan.buffer_mutex;
       rst)
  else
    (Mutex.unlock chan.buffer_mutex;
     if chan.closed_flag then
       EofObject
     else
       (maintain_clients_count chan (-1);
        Event.sync (Event.receive chan.channel)))
;;

let close chan =
  with_mutex chan.closed_flag_mutex
    (fun () ->
       if chan.closed_flag then
         closed_chan chan
       else
         chan.closed_flag <- true;
    )
;;
