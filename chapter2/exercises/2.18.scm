(define (my-reverse list-in)
  (if (null? list-in)
    '()
    (append (my-reverse (cdr list-in))
          (list (car list-in)))))

(my-reverse (list 1 2 3 4 5 6 7 8))
