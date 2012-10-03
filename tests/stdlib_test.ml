open Test

let _ =
  test "(abs -4) (abs 0) (abs 3)" "4\n0\n3";
  test "(list)" "()";
  test "(list 1 2 3)" "(1 2 3)";
  test "(list (+ 1 2) 3)" "(3 3)";
  test "(map (curry + 2) '(1 2 3 4))" "(3 4 5 6)";
  test "(filter even? '(1 2 3 4))" "(2 4)";
  test "(gcd 32 -36) (gcd)" "4\n0";
  test "(lcm 32 -36) (lcm)" "288\n1";

  prerr_string "All passed!\n"
