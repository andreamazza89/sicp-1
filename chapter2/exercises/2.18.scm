(define (my-reverse list-in)
  (if (null? list-in)
    '()
    (cons (my-reverse (cdr list-in))
          (car list-in))))

(my-reverse (list 1 2 3 4 5 6 7 8))
