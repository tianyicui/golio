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

let create () = {
  id = new_id ();
  channel = Event.new_channel ();
}
;;
let send chan value =
  Event.sync (Event.send chan.channel value)
;;
let receive chan =
  Event.sync (Event.receive chan.channel)
;;
