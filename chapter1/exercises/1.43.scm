(define (repeated f count)
  (lambda (x)
    (if (= count 0)
      x
      ((repeated f (- count 1)) (f x)))))

((repeated square 2) 5)
