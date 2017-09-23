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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rect-area x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (define (predicate-within-bounds)
    (predicate (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo trials predicate-within-bounds)
     (rect-area x1 x2 y1 y2)))

(define (simple-rectangle x y)
  #t)

(define (complex-rectangle x y)
  (and (< x 2) (< y 2)))

(define (circle x y)
  (<= (+ (sqr (- x 500))
         (sqr (- y 500)))
      (sqr 500)))

(assert-equals "finds the area of a rectangle as large as the bounds"
  2
  (estimate-integral simple-rectangle 0 1 0 2 10000))

(assert-equals "finds the are a of a rectangle as large as the bounds"
  4
  (exact->inexact (estimate-integral complex-rectangle 0 10 0 10 100000)))

(assert-equals "finds the are a of a circle smaller than the bounds"
  pi
  (exact->inexact (/ (estimate-integral circle 0 1000 0 1000 100000) (sqr 500))))
