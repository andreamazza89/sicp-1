(load "little-test.rkt")

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define current-rand 42)
(define (rand action)
  (cond
    ((eq? 'generate action)
     (let ((new-rand (rand-update current-rand)))
       (set! current-rand new-rand)
       new-rand))
    ((eq? 'reset action) (lambda (new-seed) (set! current-rand new-seed)))
    (else (error "unsupported action"))))

(assert-equals "generates rand with default seed"
  17
  (rand 'generate))

(assert-equals "generates another rand"
  104
  (rand 'generate))

((rand 'reset) 42)
(assert-equals "seed can be set"
  17
  (rand 'generate))

(assert-equals "and another rand can be generated from that seed"
  104
  (rand 'generate))

((rand 'reset) 1231)
(assert-equals "different seed gives a different first rand"
  116
  (rand 'generate))
