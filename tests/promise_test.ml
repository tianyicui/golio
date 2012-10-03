open Type
open Test

let _ =
  test "(define f (delay (newline))) f (force f) f"
    "#<promise>\n\n#<promise!#<void>>";
  test "(define f (delay (+ 1 1))) f (force f) f"
    "#<promise>\n2\n#<promise!2>";
  test "(define f (delay (write 1))) (write 2) (force f)" "21";
  test "(define f (delay (write 1))) (force f) (force f)" "1";

  prerr_string "All passed!\n"
;;
