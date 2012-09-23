let _ =
    let test str rst =
        assert (Helper.repl_str str = rst)
    in

    let test_exn str expected =
        try ignore (Helper.repl_str str) with
        | catched -> assert (catched = expected)
    in

    test "(begin 1 2 3)" "3";
    test "(define x 1) (begin (set! x 2) x)" "2";

    test "(if 1 2 invalid)" "2";
    test "(if #f invalid 'ok)" "ok";

    test "(define x 3) x (+ x x)" "3\n6";
    test "(define x 1) x (define x (+ x 1)) x" "1\n2";
    test "(define str \"A string\") (string<? str \"The string\")" "#t";
    (* TODO: internal define *)
    test_exn "(+ y 2)" (Failure "get_var: cannot get undefined variable");

    test "(define x -2) x (set! x (* x x)) x" "-2\n4";
    test_exn "(set! x 1)" (Failure "set_var: cannot set undefined variable");

    Printf.printf "All passed!\n"
