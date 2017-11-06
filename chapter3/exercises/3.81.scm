(define seed 42)

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define sample-requests (stream 'generate 'generate 'generate 'reset 'generate))

(define (my-rand requests)
  (define rands
    (cons-stream
      (rand-update seed)
      (stream-map
        (lambda (request current-rand)
          (if (eq? 'generate request)
            (rand-update current-rand)
            (rand-update seed)))
          (stream-cdr requests)
          rands)))
  rands)

(display (stream-ref (my-rand sample-requests) 0))
(newline)
(display (stream-ref (my-rand sample-requests) 1))
(newline)
(display (stream-ref (my-rand sample-requests) 2))
(newline)
(display (stream-ref (my-rand sample-requests) 3))
(newline)
(display (stream-ref (my-rand sample-requests) 4))
(newline)
