open Type

let id_count =
  ref 0
;;

let new_id () =
  let id = !id_count in
    id_count := id + 1;
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
