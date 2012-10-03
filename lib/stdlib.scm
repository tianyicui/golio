(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))
(define (list . objs) objs)

(define (id obj) obj)
(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g) (lambda (arg) (f (apply g arg))))

; TODO: quatient, remainder, modulo
(define remainder %)
(define quotient /)
(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (even? num) (= (% num 2) 0))
(define (odd? num) (not (even? num)))
(define (abs num) (if (negative? num) (- num) num))
; from tinyscheme
(define gcd
  (lambda a
    (if (null? a)
      0
      (let ((aa (abs (car a)))
            (bb (abs (cadr a))))
         (if (= bb 0)
              aa
              (gcd bb (% aa bb)))))))
; from tinyscheme
(define lcm
  (lambda a
    (if (null? a)
      1
      (let ((aa (abs (car a)))
            (bb (abs (cadr a))))
         (if (or (= aa 0) (= bb 0))
             0
             (abs (* (quotient aa (gcd aa bb)) bb)))))))
; TODO: number->string, string->number

; TODO: set-car!, set-cdr!
; TODO: append
; from tinyscheme
(define (list-tail x k)
    (if (zero? k)
        x
        (list-tail (cdr x) (- k 1))))
; from tinyscheme
(define (list-ref x k)
    (car (list-tail x k)))
(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))
(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))
(define fold foldl)
(define reduce fold)
(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))
(define (sum . lst) (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
; should be implemented as macro for short-circuit
(define (and . lst) (fold && #t lst))
(define (or . lst) (fold || #f lst))
(define (max first . rest) (fold (lambda (old new) (if (> old new) old new)) first rest))
(define (min first . rest) (fold (lambda (old new) (if (< old new) old new)) first rest))
(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))
(define (reverse lst) (fold (flip cons) '() lst))
(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst) (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist) (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist) (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))
; TODO map can have multiple args
(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))
; TODO for-each
(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

; TODO string-length, string-ref, string-set!, string-ci=? and co, substring, string-append, string-copy, string-fill!
