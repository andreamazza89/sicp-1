(define sense-data (stream 1 2 -2 -3 -4 4 -5 -3 -2 2 2 2))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
        the-empty-stream
      (cons-stream
				(apply proc (map stream-car argstreams))
        (apply stream-map
					(cons proc (map stream-cdr argstreams))))))

(define (sign-change-detector before now)
  (cond
    ((and (< now 0) (> before 0)) -1)
    ((and (> now 0) (< before 0)) 1)
    (else 0)))

(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (stream-cdr sense-data)))


(define (show-stream-until-nth stream n)
  (define (loop current)
    (cond ((stream-null? stream) 'nothing-to-show)
          ((= current n) 'done)
          (else
            (display (stream-ref stream current))
            (newline)
            (loop (+ current 1)))))
  (loop 0))

(show-stream-until-nth zero-crossings 10)
