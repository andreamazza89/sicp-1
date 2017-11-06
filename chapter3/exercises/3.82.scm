(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (rect-area x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))

(define (estimate-integral predicate x1 x2 y1 y2)
  (define (experiment-stream)
    (cons-stream
      (predicate (random-in-range x1 x2) (random-in-range y1 y2))
        (experiment-stream)))
  (stream-map
    (lambda (percentage) (* percentage (rect-area x1 x2 y1 y2)))
    (monte-carlo (experiment-stream) 0 0)))

(define (show-stream-until-nth stream n)
  (define (loop current)
    (cond ((stream-null? stream) 'nothing-to-show)
          ((= current n) 'done)
          (else
            (display (exact->inexact (stream-ref stream current)))
            (newline)
            (loop (+ current 1)))))
  (loop 0))

(define (complex-rectangle x y)
  (and (< x 2) (< y 2)))

(show-stream-until-nth (estimate-integral complex-rectangle 0 10 0 10) 7000)
