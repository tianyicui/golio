(define (gen ch n)
  (let loop ((i 1))
    (if (<= i n)
      (begin
        (send ch i)
        (loop (+ i 1)))
      (close-chan ch))))

(define (pipe in out)
  (let ((x (receive in)))
    (if (eof-object? x)
      (close-chan out)
      (begin
        (send out x)
        (pipe in out)))))

(define (print ch)
  (let ((x (receive ch)))
    (if (not (eof-object? x))
      (begin
        (write x)
        (newline)
        (print ch)))))

(define input (make-chan))
(define output (make-chan))
(go (gen input 100)
    (print output))
(let loop ((in input) (i 1))
  (if (<= i 100)
    (let ((out (make-chan)))
      (go (pipe in out))
      (loop out (+ i 1)))
    (go (pipe in output))))
