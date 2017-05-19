(define (square x)
  (* x x))

(define (double x)
  (* 2 x))

(define (improve guess x)
  (/
    (+
      (/ x (square guess))
      (double guess))
    3))

(define (new-good-enough? previous-guess current-guess)
  (< (/ (abs (- previous-guess current-guess)) previous-guess) 0.001))

(define (cubert-iter current-guess previous-guess x)
  (if (new-good-enough? previous-guess current-guess)
      current-guess
      (cubert-iter (improve current-guess x) current-guess x)))

(define (cubert x)
  (cubert-iter 1.0 0 x))

(cubert 9)
