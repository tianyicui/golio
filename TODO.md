* current
    * continuous refactor
    * GOROUTINES AND CHANNELS!
        * more and more tests
        * why even `l=1024K` (originally `l=64K` is fine) will cause stack overflow in `(factorial 10)`?
        * when a thread is permanently blocked, currently we cannot make it exit
        * combining exception handling and deadlock detection

* later
    * add `golio_test.ml`...
    * refactor quasiquote
    * A global symbol table (using Weak)
    * better exception
    * user-defined macro
    * more complete and compliant stdlib and tests
        * more I/O functions
        * `eval`
        * `delay` and `force`
        * vector
    * OASIS
    * `call-with-current-continuation` and `dynaic-wind`?
