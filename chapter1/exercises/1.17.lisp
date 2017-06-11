(define (double a)
  (* 2 a))

(define (halve a)
  (/ a 2))

(define (times a b)
  (cond
    [(= b 0) 0]
    [(even? b) (double (times a (halve b)))]
    [else (+ a (times a (- b 1)))]))

(print "---BEGIN TESTS---")
(print "four times five")
(= 20 (times 4 5))
(times 4 5)

(print "three times three")
(= 9 (times 3 3))
