(define ping-chan (make-chan))
(define pong-chan (make-chan))

(define (ping n)
  (if (/= n 0)
    (begin
      (write (receive ping-chan))
      (newline)
      (send pong-chan 'pong)
      (ping (- n 1)))))

(define (pong n)
  (if (/= n 0)
    (begin
      (send ping-chan 'ping)
      (write (receive pong-chan))
      (newline)
      (pong (- n 1)))))

(go (ping 5) (pong 5))
