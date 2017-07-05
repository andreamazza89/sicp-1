(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

;; test
(define (inc x)
  (+ x 1))

(= 55 (sum identity 0 inc 10))
