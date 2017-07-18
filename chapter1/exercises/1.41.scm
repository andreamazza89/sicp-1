(define (double f)
  (lambda (input)
    (f (f input))))

(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5)
