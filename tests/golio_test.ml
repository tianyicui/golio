let test param input =
  let (o,i,e) =
    Unix.open_process_full ("./golio " ^ param) [| |]
  in
    output_string i input;
    close_out i;
    let rec go () =
      try
        let ch = input_char o in (String.make 1 ch) ^ (go ())
      with
        | End_of_file -> ""
    in
    let rst = go () in
      assert (Unix.close_process_full (o,i,e) = (Unix.WEXITED 0));
      rst
;;

let _ =

  ignore (test "-h" "");
  ignore (test "-c '(write 1)'" "1");
  assert (test "" "1\n2" = "> 1\n> 2\n> ");
  assert (test "-" "(write 1) 2" = "1");

  prerr_endline "All passed!"
