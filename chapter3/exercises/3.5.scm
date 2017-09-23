(load "little-test.rkt")

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (rect-area x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (* 1 (rect-area x1 x2 y1 y2)))

(define (simple-rectangle x y)
  #t)

(assert-equals "finds the are a of a rectangle as large as the bounds"
  2
  (estimate-integral simple-rectangle 0 1 0 2 100))
