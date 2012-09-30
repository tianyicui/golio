open Type

let id_count =
  ref 0
;;
let id_cnt_mutex =
  Mutex.create ()
;;
let reset_count () =
  Mutex.lock id_cnt_mutex;
  id_count := 0;
  Mutex.unlock id_cnt_mutex
;;
let new_id () =
  Mutex.lock id_cnt_mutex;
  let id = !id_count in
    id_count := id + 1;
    Mutex.unlock id_cnt_mutex;
    id
;;

let create capacity = {
  id = new_id ();
  channel = Event.new_channel ();
  capacity = capacity;
  buffer = Q.create ();
  buffer_mutex = Mutex.create ();
}
;;
let lock_buffer chan =
  Mutex.lock chan.buffer_mutex;
;;
let unlock_buffer chan =
  Mutex.unlock chan.buffer_mutex;
;;
let send chan value =
  lock_buffer chan;
  if Q.length chan.buffer < chan.capacity then
    (Q.push value chan.buffer;
     unlock_buffer chan)
  else
    (unlock_buffer chan;
     Event.sync (Event.send chan.channel value))
;;
let receive chan =
  lock_buffer chan;
  if not (Q.is_empty chan.buffer) then
    (let rst = Q.pop chan.buffer in
       unlock_buffer chan;
       rst)
  else
    (unlock_buffer chan;
     Event.sync (Event.receive chan.channel))
;;
