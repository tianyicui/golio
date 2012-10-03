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

let lock_buffer chan =
  Mutex.lock chan.buffer_mutex
;;
let unlock_buffer chan =
  Mutex.unlock chan.buffer_mutex
;;
let lock_clients_count chan =
  Mutex.lock chan.clients_count_mutex
;;
let unlock_clients_count chan =
  Mutex.unlock chan.clients_count_mutex
;;
let lock_closed_flag chan =
  Mutex.lock chan.closed_flag_mutex
;;
let unlock_closed_flag chan =
  Mutex.unlock chan.closed_flag_mutex
;;

let maintain_clients_count chan delta event =
  lock_clients_count chan;
  let will_block = (delta * chan.clients_count >= 0) in
    (if will_block then
       (chan.clients_count <- chan.clients_count + delta;
        Runtime.Fiber.blocked ())
     else
       (chan.clients_count <- chan.clients_count - delta;
        Runtime.Fiber.unblocked ()));
    unlock_clients_count chan;
    Event.sync event
;;

let send chan value =
  lock_closed_flag chan;
  if chan.closed_flag then closed_chan chan;
  unlock_closed_flag chan;

  lock_buffer chan;
  if Q.length chan.buffer < chan.capacity then
    (Q.push value chan.buffer;
     unlock_buffer chan)
  else
    (unlock_buffer chan;
     maintain_clients_count chan 1
       (Event.send chan.channel value))
;;

let receive chan =
  lock_buffer chan;
  if not (Q.is_empty chan.buffer) then
    (let rst = Q.pop chan.buffer in
       unlock_buffer chan;
       rst)
  else
    (unlock_buffer chan;
     if chan.closed_flag then
       EofObject
     else
       maintain_clients_count chan (-1)
         (Event.receive chan.channel))
;;

let close chan =
  lock_closed_flag chan;
  if chan.closed_flag then
    closed_chan chan
  else
    chan.closed_flag <- true;
  unlock_closed_flag chan
;;
