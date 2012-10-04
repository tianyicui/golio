; ported from go/tests/chan/fifo.go

(define N 10)
(define (asynch-fifo)
  (let ((ch (make-chan N)))
       (let loop ((i 0))
            (if (< i N)
                (begin
                  (send ch i)
                  (loop (+ i 1)))))
       (let loop ((i 0))
            (if (< i N)
                (begin
                  (if (/= (receive ch) i)
                      (panic!))
                  (loop (+ i 1)))))))

(define (chain ch val in out)
  (receive in)
  (if (/= (receive ch) val) (panic!))
  (send out 1))

(define (sync-fifo)
  (let* ((ch (make-chan))
         (in (make-chan))
         (start in)
         (end in))
    (let loop ((i 0) (in in))
      (if (< i N)
          (let ((out (make-chan)))
            (go (chain ch i in out))
            (set! end out)
            (loop (+ i 1) out))))
    (send start 0)
    (let loop ((i 0))
      (if (< i N)
          (begin
            (send ch i)
            (loop (+ i 1)))))
    (receive end)))

(go
  ;; TODO (asynch-fifo)
  (sync-fifo))
