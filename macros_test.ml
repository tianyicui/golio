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

    test "(lambda x 1 2 3)" "(lambda x 1 2 3)";
    test "(lambda (x) 1 2 3)" "(lambda (x) 1 2 3)";
    test "(lambda (x y) 1 2 3)" "(lambda (x y) 1 2 3)";
    test "(lambda (x . y) 1 2 3)" "(lambda (x . y) 1 2 3)";
    test "(lambda (x y . z) 1 2 3)" "(lambda (x y . z) 1 2 3)";
    test "((lambda (x) x) 'a)" "a";
    test "((lambda x x) 'a)" "(a)";
    test "((lambda x x) 'a 'b)" "(a b)";
    test "((lambda (x y) (+ x y)) 3 5)" "8";
    test "((lambda x (apply + x)) 1 2 3)" "6";
    test "((lambda (x . y) (+ x (car y))) 1 2 5)" "3";
    test "((lambda (x y . z) (+ x y (car z))) 1 2 5 11)" "8";
    test "(define x 10) ((lambda (x) x) 5) x" "5\n10";

    Printf.printf "All passed!\n"
