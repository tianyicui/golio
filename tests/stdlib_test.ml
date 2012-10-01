open Test

let _ =
  test "(map (curry + 2) '(1 2 3 4))" "(3 4 5 6)";
  test "(filter even? '(1 2 3 4))" "(2 4)";

  prerr_string "All passed!\n"
