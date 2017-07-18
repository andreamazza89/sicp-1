(define (filtered-accumulate combiner use? null-value term a next b)
  (cond
    ((> a b)
      null-value)
    ((use? a)
      (combiner
        (term a)
        (filtered-accumulate combiner use? null-value term (next a) next b)))
    (else
      (filtered-accumulate combiner use? null-value term (next a) next b))))

(define (inc x) (+ x 1))

(filtered-accumulate + even? 0 identity 1 inc 4)
(filtered-accumulate + odd? 0 identity 1 inc 5)
