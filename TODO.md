* continuous refactor
    * better exception
    * in macro implementation, the syntax parsing and actual implementation should be seperated?

* standard library

* quasiquote and user-defined macro

* GOROUTINES AND CHANNELS!

* features
    * `eval`
    * `delay` and `force`
    * add tail recursion? test case: `(define (f x) (if (= x 0) 0 (f (- x 1)))) (f 100000)`
    * `call-with-current-continuation` and `dynaic-wind`?
