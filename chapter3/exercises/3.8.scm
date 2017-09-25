(load "little-test.rkt")

(define some-state -1)

(define (f n)
  (let ((result (+ n some-state)))
    (set! some-state n)
    result))

(assert-equals "left to right gives zero"
  0
  (+ (f 0) (f 1)))

;;;; (reset state)
(set! some-state -1)
(assert-equals "right to left gives one"
  1
  (+ (f 1) (f 0)))
