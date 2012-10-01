open Type
open Test

let test_arg_count_mismatch str expected got =
  test_exn str
    (get_exn (fun () -> arg_count_mismatch expected got))
;;

let test_not_applicable str value =
  test_exn str
    (get_exn (fun () -> not_applicable value))
;;

let test_unbound_var str var =
  test_exn str
    (get_exn (fun () -> unbound_var var))
;;

let _ =
  test_arg_count_mismatch "(cons 1 2 3)" "2" 3;
  test_arg_count_mismatch "(cons 1)" "2" 1;
  test_arg_count_mismatch "(/)" "1+" 0;
  test_arg_count_mismatch "(symbol?)" "1" 0;
  test_arg_count_mismatch "(symbol? 1 2 3)" "1" 3;
  test_arg_count_mismatch "(open-input-file)" "1" 0;
  test_arg_count_mismatch "(open-input-file 1 2)" "1" 2;
  test_arg_count_mismatch "(read 1 2)" "0 or 1" 2;
  test_arg_count_mismatch "(write)" "1 or 2" 0;
  test_arg_count_mismatch "(write 1 2 3)" "1 or 2" 3;
  test_arg_count_mismatch "(apply min)" "2+" 1;
  test_arg_count_mismatch "(make-chan 1 2)" "0 or 1" 2;

  test_arg_count_mismatch "((lambda () 1) 1)" "0" 1;
  test_arg_count_mismatch "((lambda () 1) 1 2)" "0" 2;
  test_arg_count_mismatch "((lambda (x) (+ x 1)))" "1" 0;
  test_arg_count_mismatch "((lambda (x) (+ x 1)) 1 2)" "1" 2;
  test_arg_count_mismatch "((lambda (x y) (+ x y)) 1)" "2" 1;
  test_arg_count_mismatch "((lambda (x y) (+ x y)) 1 2 3)" "2" 3;
  test_arg_count_mismatch "((lambda (x . y) (cons x y)))" "1+" 0;
  test_arg_count_mismatch "((lambda (x y . z) (cons (+ x y) z)) 1)" "2+" 1;
  test_arg_count_mismatch "(define (f) 1) (f 1)" "0" 1;
  test_arg_count_mismatch "(define (f) 1) (f 1 2)" "0" 2;
  test_arg_count_mismatch "(define (f x) (+ x 1)) (f)" "1" 0;
  test_arg_count_mismatch "(define (f x) (+ x 1)) (f 1 2)" "1" 2;
  test_arg_count_mismatch "(define (f x y) (+ x y)) (f 1)" "2" 1;
  test_arg_count_mismatch "(define (f x y) (+ x y)) (f 1 2 3)" "2" 3;
  test_arg_count_mismatch "(define (f x . y) (cons x y)) (f)" "1+" 0;
  test_arg_count_mismatch "(define (f x y . z) (cons (+ x y) z)) (f 1)" "2+" 1;

  test_not_applicable "(1 2)" (number 1);
  test_not_applicable "(define x 1) (x 2)" (number 1);
  test_not_applicable "('(1 2 3) 2 3)" (list_ [number 1; number 2; number 3]);

  test_unbound_var "(+ x 1)" "x";
  test_unbound_var "(set! x 1)" "x";

  prerr_string "All passed!\n"
