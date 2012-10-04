open Type

let create () = {
  id = Runtime.Chan_id.gen ();
  channel = Event.new_channel ();
  closed_flag = false;
  clients_count = 0;
  mutex = Mutex.create ();
}
;;

let maintain_clients_count chan delta =
  let will_block = (delta * chan.clients_count >= 0) in
    (if will_block then
       (chan.clients_count <- chan.clients_count + delta;
        Runtime.Fiber.blocked ())
     else
       (chan.clients_count <- chan.clients_count + delta;
        Runtime.Fiber.unblocked ()));
;;

let unblocked_send_impl chan value =
  if None <> Event.poll (Event.send chan.channel value) then
    (maintain_clients_count chan 1;
     true)
  else
    false
;;

let unblocked_send chan value =
  Mutex.lock chan.mutex;
  if chan.closed_flag then
    (Mutex.unlock chan.mutex;
     false)
  else
    let rst = unblocked_send_impl chan value in
      Mutex.unlock chan.mutex;
      rst
;;

let send chan value =
  Mutex.lock chan.mutex;
  if chan.closed_flag then
    (Mutex.unlock chan.mutex;
     closed_chan chan);
  if unblocked_send_impl chan value then
    Mutex.unlock chan.mutex
  else
    (maintain_clients_count chan 1;
     Mutex.unlock chan.mutex;
     Event.sync (Event.send chan.channel value))
;;

let unblocked_receive_impl chan =
  if chan.closed_flag then
    Some EofObject
  else
    (let rst = Event.poll (Event.receive chan.channel) in
       if None <> rst then
         maintain_clients_count chan (-1);
       rst)
;;

let unblocked_receive chan =
  Mutex.lock chan.mutex;
  if chan.closed_flag then
    (Mutex.unlock chan.mutex;
     None)
  else
    let rst = unblocked_receive_impl chan in
      Mutex.unlock chan.mutex;
      rst
;;

let receive chan =
  Mutex.lock chan.mutex;
  match unblocked_receive_impl chan with
    | Some value ->
        (Mutex.unlock chan.mutex;
         value)
    | None ->
        (maintain_clients_count chan (-1);
         Mutex.unlock chan.mutex;
         Event.sync (Event.receive chan.channel))
;;

let close chan =
  Mutex.lock chan.mutex;
  if chan.closed_flag then
    (Mutex.unlock chan.mutex;
     closed_chan chan)
  else
    chan.closed_flag <- true;
  (* After a chan got closed, all fiber waiting to receive from it should
   * be unblocked notified with EofObject *)
  if chan.clients_count > 0 then
    closed_chan chan
  else
    for i = 1 to (- chan.clients_count) do
      Event.sync (Event.send chan.channel EofObject);
      Runtime.Fiber.unblocked ()
    done;
  Mutex.unlock chan.mutex;
;;
