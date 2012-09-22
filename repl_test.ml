let _ =
    let test str rst =
        assert (Helper.repl_str str = rst)
    in

    test "\"abc\"" "\"abc\"";

    test "1" "1";
    test "(+ 1 1)" "2";
    test "(+ 1 2 -3)" "0";
    test "(+ (* 3 4) (- -4 5) (/ 2 -1) (% 6 3))" "1";

    test "(= 1 2) (= 1 1)" "#f\n#t";
    test "(< 1 2) (< 1 1)" "#t\n#f";
    test "(> 1 1) (> 2 1)" "#f\n#t";
    test "(/= 1 1) (/= 1 2)" "#f\n#t";
    test "(>= 1 1) (>= 1 2)" "#t\n#f";
    test "(<= 1 1) (<= 2 1)" "#t\n#f";

    test "(&& #t #f) (&& #t #t)" "#f\n#t";
    test "(|| #t #f) (|| #f #f)" "#t\n#f";

    test "(string=? \"a\" \"b\") (string=? \"a\" \"a\")" "#f\n#t";
    test "(string<? \"a\" \"b\") (string<? \"a\" \"a\")" "#t\n#f";
    test "(string>? \"b\" \"b\") (string>? \"b\" \"a\")" "#f\n#t";
    test "(string<=? \"a\" \"b\") (string<=? \"a\" \"a\")" "#t\n#t";
    test "(string>=? \"a\" \"b\") (string>=? \"a\" \"a\")" "#f\n#t";

    test "(symbol? #f) (symbol? 'a)" "#f\n#t";
    test "(string? 'a) (string? \"a\")" "#f\n#t";
    test "(number? #f) (number? 1)" "#f\n#t";
    test "(bool? 't) (bool? #t)" "#f\n#t";
    test "(list? '()) (list? '(a)) (list? '(a b))" "#t\n#t\n#t";
    test "(list? '(a . b)) (list? 2)" "#t\n#f";

    test "(symbol->string 'a) (string->symbol \"a\")" "\"a\"\na";

    test "(car '(a b c))" "a";
    test "(car '(a))" "a";
    test "(car '(a b . c))" "a";
    test "(cdr '(a b c))" "(b c)";
    test "(cdr '(a b))" "(b)";
    test "(cdr '(a))" "()";
    test "(cdr '(a . b))" "b";
    test "(cdr '(a b . c))" "(b . c)";
    test "(cons 'a '(b c))" "(a b c)";
    test "(cons 'a '())" "(a)";
    test "(cons 'a '(b . c))" "(a b . c)";
    test "(cons 'a 'b)" "(a . b)";
    test "(cons '() '())" "(())";

    test "(eqv? 'a 'a)" "#t";
    test "(eqv? 'a 'b)" "#f";
    test "(eqv? 2 2)" "#t";
    test "(eqv? '() '())" "#t";
    test "(eqv? (cons 1 2) (cons 1 2))" "#f";
    test "(eqv? #f 'nil)" "#f";
    (* TODO test "(let ((p (lambda (x) x))) (eqv? p p))" "#t"; *)
    test "(eq? 'a 'a)" "#t";
    (* TODO test "(eq? (list 'a) (list 'a))" "#f" *)
    test "(eq? '() '())" "#t";
    (* TODO test "(eq? car car)" "#t"; *)
    test "(equal? 'a 'a)" "#t";
    test "(equal? '(a) '(a))" "#t";
    test "(equal? '(a (b) c) '(a (b) c))" "#t";
    test "(equal? \"abc\" \"abc\")" "#t";
    test "(equal? 2 2)" "#t";

    test "'abc" "abc";
    test "(quote abc)" "abc";
    test "'()" "()";
    test "'(compose f g)" "(compose f g)";

    test "(if 1 2 invalid)" "2";
    test "(if #f invalid 'ok)" "ok";

    Printf.printf "All passed!\n"
