(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

(stream-ref y 7)
(display-stream z)

; What is the value of sum after each of the above expressions is evaluated?

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; after this line, accum is used once, just to evaluate the car of seq, so sum is 1

(define y (stream-filter even? seq))
; this keeps forcing evaluation until it finds an even number starting from 1, which requires two calls to accum,
; making sum 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
; this keeps forcing evaluation until it finds a multiple of 5, which takes three callse, making sum 15

; gonna sto here as this seems unnecessarily complicated. I do not want to learn how to deal with such
; gross complication.
