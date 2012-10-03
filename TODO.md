* current
    * continuous refactor
    * GOROUTINES AND CHANNELS!
        * more and more tests

* 0.1
    * add `golio_test.ml`...
    * more complete and compliant stdlib and tests
        * more I/O functions
        * `eval`
    * ocamldoc
    * OASIS

* later
    * Use `Event.select` to implement a high-perf `select` macro
    * A global symbol table (using Weak)
    * user-defined macro
    * refactor `quasiquote`
    * `cond`, `case`, `and`, `or`
    * vector
    * why even `l=1024K` (originally `l=64K` is fine) will cause stack overflow in `(factorial 10)`?

* long term
    * better exception: where the error is
    * `call-with-current-continuation` and `dynaic-wind`
    * OCaml Companion Tools (doc, lint, coverage, etc.)
    * Use GADTs to improve type
