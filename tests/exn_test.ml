open Type
open Test

let test_arg_count_mismatch str expected got =
  test_exn str
    (get_exn (fun () -> arg_count_mismatch expected got))
;;

let test_arg_type_mismatch str expected got =
  test_exn str
    (get_exn (fun () -> arg_type_mismatch expected got))
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
  test_arg_count_mismatch "(newline 1 2)" "0 or 1" 2;
  test_arg_count_mismatch "(apply min)" "2+" 1;
  (* TODO make-chan with 1 param *)
  test_arg_count_mismatch "(make-chan 1 2)" "0" 2;

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

  test_arg_count_mismatch "(if)" "2 or 3" 0;
  test_arg_count_mismatch "(if #t)" "2 or 3" 1;
  test_arg_count_mismatch "(if #t 1 2 3)" "2 or 3" 4;
  test_arg_count_mismatch "(set! x)" "2" 1;
  test_arg_count_mismatch "(set! x 1 2)" "2" 3;
  test_arg_count_mismatch "(quasiquote (x) 1)" "1" 2;
  test_arg_count_mismatch "`((unquote (f 1) 2))" "1" 2;
  test_arg_count_mismatch "(delay)" "1" 0;
  test_arg_count_mismatch "(delay 1 2)" "1" 2;
  test_arg_count_mismatch "(load \"file1\" \"file2\")" "1" 2;

  test_arg_type_mismatch "(car 1)" "pair" (number 1);
  test_arg_type_mismatch "(cdr 1)" "pair" (number 1);
  test_arg_type_mismatch "(car (read))" "sexp" EofObject;
  test_arg_type_mismatch "(apply 1 2)" "procedure" (number 1);
  (* TODO test_arg_type_mismatch "(make-chan 'a)" "number" (symbol "a"); *)
  test_arg_type_mismatch "(symbol->string \"a\")" "symbol" (string_ "a");
  test_arg_type_mismatch "(&& 1 'a)" "bool" (number 1);
  test_arg_type_mismatch "(load 'file)" "string" (symbol "file");
  test_arg_type_mismatch "(apply cons 'a 'b)" "list" (symbol "b");
  test_arg_type_mismatch "(send 1 2)" "chan" (number 1);
  test_arg_type_mismatch "(cons 1 . 2)" "atom or list"
    (dotted_list [symbol "cons"; number 1] (number 2));
  test_arg_type_mismatch "(force 1)" "promise" (number 1);

  test_arg_count_mismatch "(quote)" "1" 0;
  test_arg_count_mismatch "(quote 1 2)" "1" 2;

  test_not_applicable "(1 2)" (number 1);
  test_not_applicable "(define x 1) (x 2)" (number 1);
  test_not_applicable "('(1 2 3) 2 3)" (list_ [number 1; number 2; number 3]);

  test_unbound_var "(+ x 1)" "x";
  test_unbound_var "(set! x 1)" "x";

  prerr_string "All passed!\n"
