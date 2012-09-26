* refine type system, break `type sexp` into parts.
* add tail recursion, test case: `(define (f x) (if (= x 0) 0 (f (- x 1)))) (f 100000)`
