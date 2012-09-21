open Sexp

let parse str =
    Parser.parse Lexer.tokens (Lexing.from_string str)
;;

let _ =
    match parse "1" with
        [Number 1] -> ()
    ;

    match parse "test-token" with
        [Symbol "test-token"] -> ()
    ;

    match parse "()" with
        [List []] -> ()
    ;

    match parse "(make-chan)" with
        [List [Symbol "make-chan"]] -> ()
    ;

    match parse "'1" with
        [List [Symbol "quote"; Number 1]] -> ()
    ;

    match parse "'sym" with
        [List [Symbol "quote"; Symbol "sym"]] -> ()
    ;

    match parse "'()" with
        [List [Symbol "quote"; List []]] -> ()
    ;

    match parse "(+ (* 3 4) (- 4 5) (/ 2 1))" with
        [List [Symbol "+";
               List [Symbol "*"; Number 3; Number 4];
               List [Symbol "-"; Number 4; Number 5];
               List [Symbol "/"; Number 2; Number 1]]]
          -> ()
    ;

    match parse "(a)(b)" with
        [List [Symbol "a"]; List [Symbol "b"]] -> ()
    ;

    match parse "; the compose function
    (define ((compose f g) x)  (f (g x)))" with
        [List [Symbol "define";
               List [List [Symbol "compose"; Symbol "f"; Symbol "g"];
                     Symbol "x"];
               List [Symbol "f";
                     List [Symbol "g"; Symbol "x"]]]]
          -> ()
    ;

    match parse "; comment 1
    (a b) ; comment 2
    ; comment 3
    (c d) ; comment 4" with
        [List [Symbol "a"; Symbol "b"];
         List [Symbol "c"; Symbol "d"]] -> ()
    ;

    Printf.printf "All passed!\n"
