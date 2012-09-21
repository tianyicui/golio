let _ =
    let test str rst =
        assert (Helper.repl_str str = rst)
    in

    test "\"abc\"" "\"abc\"";

    test "'abc" "abc";

    test "'()" "()";

    test "'(compose f g)" "(compose f g)";

    test "1" "1";

    test "(+ 1 1)" "2";

    test "(+ 1 2 -3)" "0";

    test "(+ (* 3 4) (- -4 5) (/ 2 -1) (% 6 3))" "1";

    Printf.printf "All passed!\n"
