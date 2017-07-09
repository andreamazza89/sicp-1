;;;;;;
;;misc
(define (identity x)
  x)

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (step-two n)
  (+ n 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;linear recursive product
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

;;;;;;;;;;;;;;;;;;;
;;iterative product
(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

;;;;;;;;;;;
;;factorial
(define (factorial n)
  (product identity 1 inc n))

(define (wallis-numerator n)
  (* (product identity 4 step-two (+ n 3))
     (product identity 4 step-two (+ n 5))
     2))

(define (wallis-denominator n)
  (product square 3 step-two (+ n 4)))

;;;;;;;;;;;
;;wallis-pi
(define (wallis-pi accuracy)
  (* 4
     (/ (wallis-numerator accuracy) (wallis-denominator accuracy))))
