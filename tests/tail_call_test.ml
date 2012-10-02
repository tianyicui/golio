let _ =

  let test str rst =
    Runtime.Eval.tco := false;
    Test.test_exn str Stack_overflow;
    Runtime.Eval.tco := true;
    Test.test str rst
  in

  test "(define (f x) (if (= x 0) 0 (f (- x 1)))) (f 1024)" "0";
  test "(define (f x) (if (/= x 0) (f (- x 1)) 0)) (f 1024)" "0";
  (* TODO cond, case, and, or *)
  test "(define (f x) (if (= x 0) 0 (let ((y (- x 1))) (f y)))) (f 1024)" "0";
  test "(let loop ((x 1024)) (if (= x 0) 0 (loop (- x 1))))" "0";
  test "(define (f x) (if (= x 0) 0 (let* ((y (- x 1))) (f y)))) (f 1024)" "0";
  test "(define (f x) (if (= x 0) 0 (letrec ((y (- x 1))) (f y)))) (f 1024)" "0";
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
           (even? 1024))" "#t";
  (* TODO let-syntax, letrec-syntax *)
  test "(define (f x) (if (= x 0) 0 (begin (define y (- x 1)) (f y)))) (f 1024)" "0";
  (* TODO do *)

  prerr_string "All passed!\n"
