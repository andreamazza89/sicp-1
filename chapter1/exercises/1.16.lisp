(define (fast-expt-iter base n a)
  (cond
    [(= n 0) a]
    [(even? n) (fast-expt-iter (* base base) (/ n 2) a)]
    [else (fast-expt-iter base (- n 1) (* base a))]))
