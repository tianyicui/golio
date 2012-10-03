open Test

let _ =
  test "\"abc\"" "\"abc\"";

  test "1" "1";
  test "(+) (*)" "0\n1";
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
  test "(list? '()) (list? '(a)) (list? '(a b)) (list? '(a b . c))" "#t\n#t\n#t\n#f";
  test "(list? '(a . b)) (list? 2)" "#f\n#f";
  test "(pair? '()) (pair? '(a)) (pair? '(a b)) (pair? '(a b . c))" "#f\n#t\n#t\n#t";
  test "(pair? '(a . b)) (pair? 2)" "#t\n#f";
  test "(procedure? car) (procedure? (lambda x x)) (procedure? let)" "#t\n#t\n#f";

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
  test "(let ((p (lambda (x) x))) (eqv? p p))" "#t";
  test "(eq? 'a 'a)" "#t";
  test "(eq? (list 'a) (list 'a))" "#f";
  test "(eq? '() '())" "#t";
  test "(eq? car car)" "#t";
  test "(equal? 'a 'a)" "#t";
  test "(equal? '(a) '(a))" "#t";
  test "(equal? '(a (b) c) '(a (b) c))" "#t";
  test "(equal? \"abc\" \"abc\")" "#t";
  test "(equal? 2 2)" "#t";

  test "'abc" "abc";
  test "(quote abc)" "abc";
  test "'()" "()";
  test "'(compose f g)" "(compose f g)";

  test "(read)" "#<eof>";
  test "(read)  \n  " "#<eof>";
  test "(read)\n(1 2 3)" "(1 2 3)";
  test "(read stdin)\n#t" "#t";

  test "(write 5)" "5";
  test "(write \"\\n\")" "\"\\n\"";
  test "(write '(a b c) stdout)" "(a b c)";
  test_exn "(write (write 1))"
    (Failure "print_value: should not print Void");

  test "(newline) (write 1)" "\n1";
  test "(newline stdout) (write 1 stdout)" "\n1";

  begin
    let temp_in = Runtime.Temp_file.gen () in
    let temp_out = Runtime.Temp_file.gen () in
    let in_file_c = open_out temp_in in
      output_string in_file_c "(1 2 3)";
      close_out in_file_c;
      let program =
        Printf.sprintf
          "(define in (open-input-file  %S))
           (define out (open-output-file %S))
           (port? in)
           (input-port? in)
           (output-port? in)
           (port? out)
           (input-port? out)
           (output-port? out)

           (define x (read in))
           x
           (read in)
           (eof-object? (read in))
           (eof-object? '())
           (define y (apply + x))
           (write y out)
           (close-input-port in)
           (close-output-port out)"
          temp_in
          temp_out
      in
        test program "#t\n#t\n#f\n#t\n#f\n#t\n(1 2 3)\n#<eof>\n#t\n#f";
        let out_file_c = open_in temp_out in
          assert (input_line out_file_c = "6");
          close_in out_file_c
  end;

  test "(apply * '(2 3 4))" "24";
  test "(define compose
          (lambda (f g)
            (lambda args
              (f (apply g args)))))
        ((compose + *) 12 75)" "900";
  test "(apply min '(6 8 3 2 5))" "2";
  test "(apply min 5 1 3 '(6 8 3 2 5))" "1";

  prerr_string "All passed!\n"
