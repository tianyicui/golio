open Types

let _ =
    let test str rst =
        let open Helper in
        assert (parse_str str = rst);
        assert (parse_str (String.concat "\n" (List.map Sexp.print_sexp rst)) = rst)
    in

    test "1" [Number 1];

    test "test-token" [Symbol "test-token"];

    test "#t #f" [Bool true; Bool false];

    test "\"\\\"string\\n\"" [String "\"string\n"];

    test "()" [List []];

    test "(make-chan)" [List [Symbol "make-chan"]];

    test "'1" [List [Symbol "quote"; Number 1]];

    test "'sym" [List [Symbol "quote"; Symbol "sym"]];

    test "'()" [List [Symbol "quote"; List []]];

    test "(+ (* 3 4) (- -4 5) (/ 2 -1))"
        [List [Symbol "+";
               List [Symbol "*"; Number 3; Number 4];
               List [Symbol "-"; Number (-4); Number 5];
               List [Symbol "/"; Number 2; Number (-1)]]]
    ;

    test "(a)(b)" [List [Symbol "a"]; List [Symbol "b"]];

    test "(3 4 . 5)"
        [DottedList ([Number 3; Number 4], Number 5)];

    test "(equal? '(#t . \"5\") (cons #t \"5\"))"
        [List [Symbol "equal?";
               List [Symbol "quote";
                     DottedList ([Bool true], String "5")];
               List [Symbol "cons"; Bool true; String "5"]]]
    ;

    test "; the compose function
          (define ((compose f g) x)  (f (g x)))"
        [List [Symbol "define";
               List [List [Symbol "compose"; Symbol "f"; Symbol "g"];
                     Symbol "x"];
               List [Symbol "f";
                     List [Symbol "g"; Symbol "x"]]]]
    ;

    test "; comment 1
          (a \"b\") ; comment 2
          ; comment 3
          (c \"d\") ; comment 4"
        [List [Symbol "a"; String "b"];
         List [Symbol "c"; String "d"]]
    ;

    Printf.printf "All passed!\n"
