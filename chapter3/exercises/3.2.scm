(load "little-test.rkt")

(define (make-monitored function)
  (define calls-count 0)
  (lambda (input)
    (if (eq? 'how-many-calls? input)
      calls-count
      (begin (set! calls-count (+ calls-count 1))
             (function input)))))

(assert-equals "yields the same result as the original function"
  (sqr 5)
  ((make-monitored sqr) 5))

(assert-equals "reports no calls made to the original function"
  0
  ((make-monitored sqr) 'how-many-calls?))

(define monitored (make-monitored sqr))
(monitored 4)

(assert-equals "reports a call made to the original function"
  1
  (monitored 'how-many-calls?))
