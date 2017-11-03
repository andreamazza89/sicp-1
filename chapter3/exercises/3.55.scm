(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
        the-empty-stream
      (cons-stream
				(apply proc (map stream-car argstreams))
        (apply stream-map
					(cons proc (map stream-cdr argstreams))))))

(define (sum-streams stream1 stream2)
  (stream-map + stream1 stream2))

(define (integers first)
  (cons-stream first (integers (+ 1 first))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (partial-sums stream)
  (cons-stream
    (stream-car stream)
    (sum-streams
      (stream-cdr stream)
      (partial-sums stream))))

;;TEST;;
(display "partial sums")
(newline)
(define partial-sums-stream (partial-sums (integers 1)))
(display (= 1 (stream-ref partial-sums-stream 0)))
(newline)
(display (= 3 (stream-ref partial-sums-stream 1)))
(newline)
(display (= 6 (stream-ref partial-sums-stream 2)))
(newline)
(display (= 10 (stream-ref partial-sums-stream 3)))
(newline)
(display (= 15 (stream-ref partial-sums-stream 4)))
(newline)
(display (= 21 (stream-ref partial-sums-stream 5)))
(newline)
