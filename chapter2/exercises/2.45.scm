(define (split operation-one operation-two)
  (define (split-iter painter n)
    (if (= n 0)
      painter
      (let ((smaller (split-iter painter (- n 1))))
        (operation-two painter (operation-one smaller smaller))))))
