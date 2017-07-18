(define (inc x)
  (+ x 1))

;;linear recursive accumulate
(define (linear-accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner
      (term a)
      (linear-accumulate combiner null-value term (next a) next b))))

;;iterative accumulate
(define (iter-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter
        (next a)
        (combiner
          result
          (term a)))))
  (iter a null-value))


;;sum in terms of linear accumulate
(define (sum term a next b)
     (linear-accumulate + 0 term a next b))

;;product in terms of iter accumulate
(define (product term a next b)
  (iter-accumulate * 1 term a next b))
