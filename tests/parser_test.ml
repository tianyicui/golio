open Type
include Common

let _ =
  let parse_str str =
    let lb = Lexing.from_string str in
    let rec go () =
      match Parser.parse Lexer.tokens lb with
        | None -> []
        | Some x -> x :: go ()
    in go ()
  in

  let test str rst =
    let sexp = L.map unpack_sexp rst in
      assert (parse_str str = sexp);
      assert (parse_str (String.concat "\n" (L.map Print.print_sexp sexp)) = sexp)
  in

  test "1 -1" [number 1; number (-1)];

  test "test-token" [symbol "test-token"];
  test "test.token" [symbol "test.token"];

  test "#t #f" [bool_ true; bool_ false];

  test "\"\\\"string\\n\"" [string_ "\"string\n"];

  test "()" [list_ []];
  test "(make-chan)" [list_ [symbol "make-chan"]];

  test "'1" [list_ [symbol "quote"; number 1]];
  test "'sym" [list_ [symbol "quote"; symbol "sym"]];
  test "'()" [list_ [symbol "quote"; list_ []]];
  test "'()" [list_ [symbol "quote"; list_ []]];

  test "`()" [list_ [symbol "quasiquote"; list_ []]];

  test ",()" [list_ [symbol "unquote"; list_ []]];

  test ",@()" [list_ [symbol "unquote-splicing"; list_ []]];

  test "(+ (* 3 4) (- -4 5) (/ 2 -1))"
       [list_ [symbol "+";
               list_ [symbol "*"; number 3; number 4];
               list_ [symbol "-"; number (-4); number 5];
               list_ [symbol "/"; number 2; number (-1)]]]
  ;

  test "(a)(b)" [list_ [symbol "a"]; list_ [symbol "b"]];

  test "(3 4 . 5)"
       [dotted_list [number 3; number 4] (number 5)];

  test "(equal? '(#t . \"5\") (cons #t \"5\"))"
       [list_ [symbol "equal?";
               list_ [symbol "quote";
                      dotted_list [bool_ true] (string_ "5")];
               list_ [symbol "cons"; bool_ true; string_ "5"]]]
  ;

  test "; the compose function
        (define ((compose f g) x)  (f (g x)))"
       [list_ [symbol "define";
               list_ [list_ [symbol "compose"; symbol "f"; symbol "g"];
                      symbol "x"];
               list_ [symbol "f";
                      list_ [symbol "g"; symbol "x"]]]]
  ;

  test "; comment 1
        (a \"b\") ; comment 2
        ; comment 3
        (c \"d\") ; comment 4"
       [list_ [symbol "a"; string_ "b"];
        list_ [symbol "c"; string_ "d"]]
  ;

  prerr_string "All passed!\n"
