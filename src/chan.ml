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
            (chan.clients_count <- chan.clients_count + delta;
             Runtime.Fiber.unblocked ()));
    )
;;

let unblocked_send chan value =
  Mutex.lock chan.buffer_mutex;
  if Q.length chan.buffer < chan.capacity then
    (Q.push value chan.buffer;
     Mutex.unlock chan.buffer_mutex;
     true)
  else
    (Mutex.unlock chan.buffer_mutex;
     if None <> Event.poll (Event.send chan.channel value) then
       (maintain_clients_count chan 1;
        true)
     else false)
;;

let send chan value =
  Mutex.lock chan.closed_flag_mutex;
  if chan.closed_flag then
    (Mutex.unlock chan.closed_flag_mutex;
     closed_chan chan);
  if unblocked_send chan value then
    Mutex.unlock chan.closed_flag_mutex
  else
    (maintain_clients_count chan 1;
     Mutex.unlock chan.closed_flag_mutex;
     Event.sync (Event.send chan.channel value))
;;

let unblocked_receive chan =
  Mutex.lock chan.buffer_mutex;
  if not (Q.is_empty chan.buffer) then
    (let rst = Q.pop chan.buffer in
       Mutex.unlock chan.buffer_mutex;
       Some rst)
  else if chan.closed_flag then
    Some EofObject
  else
    (Mutex.unlock chan.buffer_mutex;
     let rst = Event.poll (Event.receive chan.channel) in
       if None <> rst then
         maintain_clients_count chan (-1);
       rst)
;;

let receive chan =
  Mutex.lock chan.closed_flag_mutex;
  match unblocked_receive chan with
    | Some value -> (Mutex.unlock chan.closed_flag_mutex; value)
    | None ->
        (maintain_clients_count chan (-1);
         Mutex.unlock chan.closed_flag_mutex;
         Event.sync (Event.receive chan.channel))
;;

let close chan =
  with_mutex chan.closed_flag_mutex
    (fun () ->
       if chan.closed_flag then
         closed_chan chan
       else
         chan.closed_flag <- true;
       (* After a chan got closed, all fiber waiting to receive from it should
        * be unblocked notified with EofObject *)
       with_mutex chan.clients_count_mutex
         (fun () ->
            if chan.clients_count > 0 then
              closed_chan chan
            else
              for i = 1 to (- chan.clients_count) do
                Event.sync (Event.send chan.channel EofObject);
                Runtime.Fiber.unblocked ()
              done);
    )
;;
