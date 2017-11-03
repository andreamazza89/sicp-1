(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
        the-empty-stream
      (cons-stream
				(apply proc (map stream-car argstreams))
        (apply stream-map
					(cons proc (map stream-cdr argstreams))))))

(define (mul-streams stream1 stream2)
  (stream-map * stream1 stream2))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (integers first)
  (cons-stream first (integers (+ 1 first))))

(define factorials
  (cons-stream 1 (mul-streams (integers 2) factorials)))

;;;TEST;;;
(display "multiplies elements of the two input streams")
(newline)
(define stream1 (cons-stream 1 (cons-stream 2 '())))
(define stream2 (cons-stream 2 (cons-stream 3 '())))
(define multiplied-stream (mul-streams stream1 stream2))
(display (= 2 (stream-ref multiplied-stream 0)))
(newline)
(display (= 6 (stream-ref multiplied-stream 1)))

(newline)
(display "factorial stream")
(newline)
(display (= 6 (stream-ref factorials 2)))
