(define (double a)
  (* 2 a))

(define (halve a)
  (/ a 2))

(define (times a b product)
  (cond
    [(= b 0) product]
    [(even? b) (times (double a) (halve b) product)]
    [else (times a (- b 1) (+ product a))]))
