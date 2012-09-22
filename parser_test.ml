open Sexp

let _ =
    let test str rst =
        let open Helper in
        assert (parse_str str = rst);
        assert (parse_str (String.concat "\n" (List.map print_sexp rst)) = rst)
    in

    test "1" [Number 1];

    test "test-token" [Atom "test-token"];

    test "#t #f" [Bool true; Bool false];

    test "\"\\\"string\\n\"" [String "\"string\n"];

    test "()" [List []];

    test "(make-chan)" [List [Atom "make-chan"]];

    test "'1" [List [Atom "quote"; Number 1]];

    test "'sym" [List [Atom "quote"; Atom "sym"]];

    test "'()" [List [Atom "quote"; List []]];

    test "(+ (* 3 4) (- -4 5) (/ 2 -1))"
        [List [Atom "+";
               List [Atom "*"; Number 3; Number 4];
               List [Atom "-"; Number (-4); Number 5];
               List [Atom "/"; Number 2; Number (-1)]]]
    ;

    test "(a)(b)" [List [Atom "a"]; List [Atom "b"]];

    test "(3 4 . 5)"
        [DottedList ([Number 3; Number 4], Number 5)];

    test "(equal? '(#t . \"5\") (cons #t \"5\"))"
        [List [Atom "equal?";
               List [Atom "quote";
                     DottedList ([Bool true], String "5")];
               List [Atom "cons"; Bool true; String "5"]]]
    ;

    test "; the compose function
          (define ((compose f g) x)  (f (g x)))"
        [List [Atom "define";
               List [List [Atom "compose"; Atom "f"; Atom "g"];
                     Atom "x"];
               List [Atom "f";
                     List [Atom "g"; Atom "x"]]]]
    ;

    test "; comment 1
          (a \"b\") ; comment 2
          ; comment 3
          (c \"d\") ; comment 4"
        [List [Atom "a"; String "b"];
         List [Atom "c"; String "d"]]
    ;

    Printf.printf "All passed!\n"
