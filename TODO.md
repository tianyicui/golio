* current
    * continuous refactor
    * GOROUTINES AND CHANNELS!
        * more and more tests, esp with `select`

* 0.1
    * ocamldoc

* 0.2
    * buffered chan

* later
    * more complete and compliant stdlib and tests
        * more I/O functions
        * see comments in `stdlib.scm`
    * audit correct usage of `top_level` in macro
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
