(define (last-pair list-in)
  (if (null? (cdr list-in))
    list-in
    (last-pair (cdr list-in))))

(last-pair (list 23 72 149 34))
