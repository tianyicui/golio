let _ =
  let test str rst =
    assert (Helper.run_str str = rst)
  in

  let test_exn str expected =
    try (ignore (Helper.run_str str); assert false)
    with catched -> assert (catched = expected)
  in

  test "(begin 1 2 3)" "3";
  test "(define x 1) (begin (set! x 2) x)" "2";
  test "(begin (define x 1)) x" "1";

  test "(if 1 2 invalid)" "2";
  test "(if #f invalid 'ok)" "ok";
  test "(if #t 1)" "1";
  test "(if #f 1)" "()";

  test "(define x 3) x (+ x x)" "3\n6";
  test "(define x 1) x (define x (+ x 1)) x" "1\n2";
  test "(define str \"A string\") (string<? str \"The string\")" "#t";
  test "(define y 2) ((lambda (x) (define y 1) (+ x y)) 3) y" "4\n2";
  test "(define x (begin (define y 2) 1)) x y" "1\n2";
  test_exn "((lambda (x) (define y 1) (+ x y)) 3) y"
      (Failure "get_var: cannot get undefined variable y");
  test_exn "(+ y 2)" (Failure "get_var: cannot get undefined variable y");
  test "(define x 0) (define z 1) (define (f x y) (set! z 2) (+ x y))
      (f 1 2) x z" "3\n0\n2";
  test "(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))
      (factorial 10)" "3628800";
  test "(define add3 (lambda (x) (+ x 3))) (add3 3)" "6";
  test "(define first car) (first '(1 2))" "1";
  test "(define (add . l) (apply + l)) (add -2 -1 0 1 2)" "0";
  test "(define (x y . z) (cons y z)) (x 1 2 3)" "(1 2 3)";
  test "(define (f x) (+ x y)) (define y 1) (f 1)" "2";
  test "(define plus (lambda (x) (+ x y))) (define y 1) (plus 3)" "4";

  test "(define x -2) x (set! x (* x x)) x" "-2\n4";
  test_exn "(set! x 1)" (Failure "set_var: cannot set undefined variable x");
  test "(define x 3) (define y 4) (let ((t x)) (set! x y) (set! y t)) x y" "4\n3";

  test "(let ((x 2) (y 3)) (* x y))" "6";
  test "(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))" "35";
  test_exn "(let () (define x 1) x) x"
    (Failure "get_var: cannot get undefined variable x");
  test "(begin (define a 5) (let ((a 10) (b a)) (- a b)))" "5";

  test "(letrec ((x 2) (y 3)) (* x y))" "6";
  test "(letrec ((x 2) (y 3)) (letrec ((x 7) (z (+ x y))) (* z x)))" "35";
  test_exn "(letrec () (define x 1) x) x"
    (Failure "get_var: cannot get undefined variable x");
  test "(define x 5) (letrec ((x 3) (y 5)) (+ x y)) x" "8\n5";
  test "(letrec ((even?
                  (lambda (n)
                    (if (eq? 0 n)
                        #t
                        (odd? (- n 1)))))
                 (odd?
                  (lambda (n)
                    (if (eq? 0 n)
                        #f
                        (even? (- n 1))))))
           (even? 88))" "#t";
  test "(begin (define a 5) (letrec ((a 10) (b a)) (- a b)))" "5";

  test "(let* ((x 2) (y 3)) (* x y))" "6";
  test "(let* ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))" "35";
  test_exn "(let* () (define x 1) x) x"
    (Failure "get_var: cannot get undefined variable x");
  test "(let* ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))" "70";
  test "(begin (define a 5) (let* ((a 10) (b a)) (- a b)))" "0";

  test "`(+ 2 3)" "(+ 2 3)";
  test "`(+ 2 ,(* 3 4))" "(+ 2 12)";
  test "`(a b (,(+ 2 3) c) d)" "(a b (5 c) d)";
  test "`(a b ,(reverse '(c d e)) f g)" "(a b (e d c) f g)";
  test "(let ((a 1) (b 2)) `(,a . ,b))" "(1 . 2)";
  test "'`,(cons 'a 'b)" "`,(cons 'a 'b)";
  test "`',(cons 'a 'b)" "'(a . b)";
  test "`,(let () (define x 1) x)" "1";
  test "``(+ ,,(+ 1 2) 2 3)" "`(+ ,3 2 3)";
  test "`(+ ,@(cdr '(* 2 3)))" "(+ 2 3)";
  test "`(a b ,@(reverse '(c d e)) f g)" "(a b e d c f g)";
  test "`(1 2 `(3 4 ,@(5 6 ,@(list 8) 9 10) 11 12) 13 14)" "(1 2 `(3 4 ,@(5 6 8 9 10) 11 12) 13 14)";
  test "(let ((a 1) (b 2)) `(,a ,@b))" "(1 . 2)";
  test "'`,(cons 'a 'b)" "`,(cons 'a 'b)";
  test "`',(cons 'a 'b)" "'(a . b)";

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
  test "(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))
        (define my-count (counter 5))
        (define your-count (counter 5))
        (my-count 3)
        (your-count 5)
        (my-count 6)
        (my-count 5)
        (your-count -1)"
       "8\n10\n14\n19\n9";

  begin
    let temp_file = Helper.temp_file () in
    let out_c = open_out temp_file in
      output_string out_c "(define x 'loaded) (define y 'ok)";
      close_out out_c;
      test (Printf.sprintf "(load %S) x y" temp_file) "loaded\nok";
  end;

  prerr_string "All passed!\n"
