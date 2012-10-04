* 0.1
    * small scope refactor
    * ocamldoc

* 0.2
    * BUG: when two fibers are using `select` to communicate to each other, they will block forever
    * IMPROVEMENT: Use `Event.select` to implement a high-perf `select` macro
    * FEATURE: dead lock detection in `select`
    * FEATURE: buffered chan
        * maybe use two `Event.channel` and a `Queue.t`
        * `Condition` module can help too
    * TASK: audit correct usage of `top_level` in macro

* later
    * more complete and compliant stdlib and tests
        * more I/O functions
        * see comments in `stdlib.scm`
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
