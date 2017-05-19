(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (new-good-enough? previous-guess current-guess)
  (< (/ (abs (- previous-guess current-guess)) previous-guess) 0.001))

(define (sqrt-iter current-guess previous-guess x)
  (if (new-good-enough? previous-guess current-guess)
      current-guess
      (sqrt-iter (improve current-guess x) current-guess x)))

(define (sqrt x)
  (sqrt-iter 1.0 0 x))
