;; send 2..n to chan `ch`
(define (gen ch n)
  (let loop ((i 2))
    (if (<= i n)
      (begin
        (send ch i)
        (loop (+ i 1)))))
  (close-chan ch))

;; receive from chan `in` and filter out all multipliers of `m`, send the rest
;; to chan `out`.
(define (filter in out m)
  (let ((x (receive in)))
    (if (eof-object? x)
      (close-chan out)
      (begin
        (if (/= 0 (% x m))
          (send out x))
        (filter in out m)))))

(define (sieve in out)
  (let ((x (receive in)))
    (if (eof-object? x)
      (close-chan out)
      (let ((ch (make-chan)))
        (send out x)
        (go (filter in ch x))
        (sieve ch out)))))

(define in (make-chan))
(define out (make-chan))
(go (gen in 1000)
    (sieve in out))
(let loop ()
  (let ((x (receive out)))
    (if (not (eof-object? x))
      (begin
        (write x)
        (newline)
        (loop)))))
