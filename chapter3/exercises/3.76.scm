(define (sign-change-detector before now)
  (cond
    ((and (< now 0) (> before 0)) -1)
    ((and (> now 0) (< before 0)) 1)
    (else 0)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
        the-empty-stream
      (cons-stream
				(apply proc (map stream-car argstreams))
        (apply stream-map
					(cons proc (map stream-cdr argstreams))))))

(define (average-curried a)
  (lambda (b)
    (/ (+ a b) 2)))

(define (smooth stream)
  (stream-map
    (average-curried (stream-car stream))
    (stream-cdr stream)))

(define (make-zero-crossings input-stream)
  (stream-map
    sign-change-detector
    (smooth input-stream)
    (stream-cdr (smooth input-stream))))
