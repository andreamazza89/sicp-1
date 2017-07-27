(define (deep-reverse list-in)
  (if (null? list-in)
    '()
    (let ((head (car list-in))
          (tail (cdr list-in)))
      (append (deep-reverse tail)
              (if (list? head)
                (deep-reverse head)
                (list head))))))

(define x (list (list 1 2) (list 3 4)))
(deep-reverse x)