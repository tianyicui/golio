open Type

let run_str ?(print_exn=true) str =
  let temp_in = Runtime.temp_file () in
  let str_c = open_out temp_in in
    output_string str_c str;
    close_out str_c;

    let in_c = open_in temp_in in
    let temp_out = Runtime.temp_file () in
    let out_c = open_out temp_out in
      (let open Repl in
         repl {
           stdin = in_c;
           stdout = out_c;
           lexbuf = None;
           interactive = false;
           print_result = true;
           print_exn = print_exn;
         });
      close_in in_c;
      close_out out_c;

      let rst_c = open_in temp_out in
      let rec go () =
        try let line = input_line rst_c in line :: go ()
        with End_of_file -> []
      in String.concat "\n" (go ())
;;

let test str rst =
  assert (run_str str = rst)
;;

let test_exn str expected =
  try
    (ignore (run_str ~print_exn:false str);
     raise Normal_exit)
  with catched ->
    if catched <> expected then
      (prerr_endline ("Got unexpected exception " ^ (Print.print_exn catched));
       raise Exit)
;;

let get_exn func =
  try func (); Failure "unreachable" with
    | expn -> expn
;;
