open Type
open Test

let _ =

  let test_time str time rst =
    let start_time = Unix.gettimeofday () in
      Test.test str rst;
      let end_time = Unix.gettimeofday () in
      let run_time = end_time -. start_time in
        assert (run_time >= time)
  in

  test "(go (write 1))" "1";
  test "(define ch (make-chan)) (go (write (receive ch)) (send ch 42))" "42";

  test_time
    "(define ch (make-chan))
     (go (write (receive ch))
         (begin
           (sleep 200)
           (send ch 42)))"
    0.2
    "42";
  test_time
    "(define ch (make-chan))
     (go (begin
           (sleep 100)
           (write (receive ch)))
         (begin
           (send ch 42)
           (sleep 100)))"
    0.2
    "42";
  test_time
    "(define ch (make-chan))
     (go (begin
           (sleep 200)
           (write (receive ch)))
         (send ch 42))"
    0.2
    "42";

  test "(define ch (make-chan 1)) (send ch 42) (write (receive ch))" "42";
  test "(define ch (make-chan 3))
        (send ch 1) (send ch 2) (send ch 3)
        (write (receive ch))
        (write (receive ch))
        (write (receive ch))" "123";

  test_exn "(define ch (make-chan)) (send ch 1)" Dead_lock;
  test_exn "(define ch (make-chan)) (receive ch)" Dead_lock;
  test_exn "(define ch (make-chan 1)) (send ch 1) (send ch 1)" Dead_lock;

  test_exn "(go (/ 1 0))" Division_by_zero;
  test_exn "(go (/ 1 0) (begin (sleep 200) x))"
    (Repl_exn [Division_by_zero; Failure "get_var: cannot get undefined variable x"]);
  test_exn "(define ch (make-chan)) (go (/ 1 0) (begin (sleep 200) x)) (receive ch)"
    (Repl_exn [Division_by_zero; Failure "get_var: cannot get undefined variable x"; Dead_lock]);
  test_exn "(define ch (make-chan)) (go (/ 1 0) (begin (sleep 200) x)) (send ch 1)"
    (Repl_exn [Division_by_zero; Failure "get_var: cannot get undefined variable x"; Dead_lock]);

  prerr_string "All passed!\n"
