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

    test "(symbol? #f) (symbol? 'a)" "#f\n#t";
    test "(string? 'a) (string? \"a\")" "#f\n#t";
    test "(number? #f) (number? 1)" "#f\n#t";
    test "(bool? 't) (bool? #t)" "#f\n#t";
    test "(list? '()) (list? '(a)) (list? '(a b))" "#t\n#t\n#t";
    test "(list? '(a . b)) (list? 2)" "#t\n#f";

    test "(symbol->string 'a) (string->symbol \"a\")" "\"a\"\na";

    Printf.printf "All passed!\n"
