(load "little-test.rkt")

(define (make-accumulator initial-count)
  (define count initial-count)
  (update-count count))

(define (update-count count)
  (lambda (delta)
    (set! count (+ count delta))
    count))

(define acc (make-accumulator 5))

(assert-equals "accumulates a number"
  10
  (acc 5))

(assert-equals "and adds onther one to it"
  17
  (acc 7))
