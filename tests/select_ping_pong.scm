(define ping-chan-0 (make-chan))
(define ping-chan-1 (make-chan))
(define ping-chan-2 (make-chan))
(define ping-chan-3 (make-chan))
(define pong-chan-0 (make-chan))
(define pong-chan-1 (make-chan))
(define pong-chan-2 (make-chan))
(define pong-chan-3 (make-chan))

(define (select-receive chan-0 chan-1 chan-2 chan-3)
  (select
    ((receive chan-0))
    ((receive chan-1))
    ((receive chan-2))
    ((receive chan-3))))

(define (random-send x . chan-list)
  (let* ((n (length chan-list))
         (i (random-integer n)))
    (send (list-ref chan-list i) x)))

(define (ping n)
  (if (/= n 0)
    (begin
      (write (select-receive ping-chan-0 ping-chan-1 ping-chan-2 ping-chan-3))
      (newline)
      (random-send 'pong pong-chan-0 pong-chan-1 pong-chan-2 pong-chan-3)
      (ping (- n 1)))))

(define (pong n)
  (if (/= n 0)
    (begin
      (random-send 'ping ping-chan-0 ping-chan-1 ping-chan-2 ping-chan-3)
      (write (select-receive pong-chan-0 pong-chan-1 pong-chan-2 pong-chan-3))
      (newline)
      (pong (- n 1)))))

(go (ping 5) (pong 5))
