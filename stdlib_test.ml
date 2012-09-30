let _ =
  let test str rst =
    assert (Helper.run_str str = rst)
  in

  test "(map (curry + 2) '(1 2 3 4))" "(3 4 5 6)";
  test "(filter even? '(1 2 3 4))" "(2 4)";

  Printf.printf "All passed!\n"
