(define (repeated f count)
  (lambda (x)
    (if (= count 0)
      x
      ((repeated f (- count 1)) (f x)))))

((repeated square 2) 5)

(define (average a b c)
  (/ (+ a b c) 3))

(define (smooth f)
  (let ((dx 0.0000001))
  (lambda (x)
    (average
      (f (- x dx))
      (f x)
      (f (+ x dx))))))

((smooth square) 4)

(define smoothie (repeated smooth 7))
((smoothie square) 4)
